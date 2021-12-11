{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Control.Monad.State
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Read (readMaybe)

type Pos = (Int, Int)
type Colony = Map Pos ([Pos], Int)

main :: IO ()
main = do
  selfCheck
  Just i <- decode <$> readFile "Day11.input"
  print $ solution1 i
  print $ solution2 i

decode :: String -> Maybe Colony
decode = fmap toMap . traverse (traverse readMaybe . map (:[])) . lines

toMap :: [[Int]] -> Colony
toMap = foldl' stepRow Map.empty . zip [0..] . map (zip [0..])
  where
    stepRow m (y, xs) = foldl' (stepCell y) m xs
    stepCell y m (x, v) = Map.insert (x, y) (neibs (x, y), v) m

neibs :: Pos -> [Pos]
neibs (x, y) =
  [ (nx, ny)
  | dx <- [-1, 0, 1]
  , dy <- [-1, 0, 1]
  , let nx = x + dx
  , let ny = y + dy
  , not (nx == x && ny == y)
  , nx >= 0 && nx <= 9
  , ny >= 0 && ny <= 9
  ]

solution1, solution2 :: Colony -> Int

solution1 c = fst $ foldl' f (0, c) [1 :: Int .. 100]
  where
    f (s, m) _ = let m' = step m in (s + zeroes m', m')
    zeroes =
      length . filter ((== 0) . snd . snd) . Map.toList

solution2 =
  fst . head . dropWhile (not . inSync . snd) . zip [0..] . iterate step
  where
    inSync = all ((== 0) . snd . snd) . Map.toList

step :: Colony -> Colony
step colony = nullify $ go (bump colony) `evalState` Set.empty
  where
    nullify = Map.map (fmap $ \v -> if v > 9 then 0 else v)
    bump = Map.map (fmap (+ 1))
    go c = do
      blinked <- get
      let ns = [ (pos, n)
               | (pos, (n, x)) <- Map.toList c
               , x > 9
               , not (Set.member pos blinked) ]
      if null ns
        then pure c
        else do
          let ps = [ p | (p, _) <- ns ]
          modify $ Set.union (Set.fromList ps)
          let as = concat [ n | (_, n) <- ns ]
          go $ foldl' (flip $ Map.update (Just . fmap (+ 1))) c as

display :: Colony -> IO ()
display c = forM_ [0..9] $ \y -> do
  forM_ [0..9] $ \x -> case Map.lookup (x, y) c of
    Just (_, v) -> putStr (show v)
    Nothing -> putStr "."
  putStrLn ""

selfCheck :: MonadFail m => m ()
selfCheck = do
  unless (solution1 example == 1656) $ fail "solution1"

example :: Colony
example =
  let Just m =
        decode $ unlines [ "5483143223"
                         , "2745854711"
                         , "5264556173"
                         , "6141336146"
                         , "6357385478"
                         , "4167524645"
                         , "2176841721"
                         , "6882881134"
                         , "4846848554"
                         , "5283751526"
                         ]
  in m
