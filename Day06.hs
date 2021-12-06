{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import Data.List
import qualified Data.IntMap.Strict as M
import Text.Read (readMaybe)

type Fish = M.IntMap Int

main = do
  selfCheck
  fs <- decode =<< readFile "Day06.input"
  let f1 = nth 80 step $ toFish fs
  let f2 = nth 176 step f1
  print $ calcFish f1
  print $ calcFish f2

decode :: MonadFail m => String -> m [Int]
decode s = maybe (fail "Bad input") pure . readMaybe $ "[" <> s <> "]"

nth :: Int -> (a -> a) -> a -> a
nth n f = head . drop n . iterate f

toFish :: [Int] -> Fish
toFish = foldl' (flip $ M.insertWith (+) `flip` 1) M.empty

step :: Fish -> Fish
step = reFish . go [] . M.toList
  where
    reFish = foldl' (flip $ uncurry (M.insertWith (+))) M.empty
    go acc []         = acc
    go acc ((x,n):xs) =
      case x of
        0 -> go ((6, n) : (8, n) : acc) xs
        _ -> go ((x - 1, n)      : acc) xs

calcFish :: Fish -> Int
calcFish = sum . map snd . M.toList

selfCheck :: IO ()
selfCheck = do
  let fs = calcFish $ nth 18 step $ toFish [3, 4, 3, 1, 2]
  unless (fs == 26) $ fail "Selfcheck"
