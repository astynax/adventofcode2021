module Main where

import Control.Monad
import Data.List
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)

type Line = ((Int, Int), (Int, Int))

main = do
  selfCheck
  ls <- traverse decode . lines =<< readFile "Day05.input"
  print $ solution1 ls
  print $ solution2 ls

solution1 = intersections . toMap . filter isVH

solution2 = intersections . toMap . filter isVHD

intersections = length . filter ((> 1) . snd) . Map.toList

toMap = foldl' step Map.empty . concatMap toCoords
  where
    step m k = Map.insertWith (+) k 1 m

isVH ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2
isVHD l@((x1, y1), (x2, y2)) = isVH l || abs (x1 - x2) == abs (y1 - y2)

toCoords ((x1, y1), (x2, y2)) = zip (range x1 x2) (range y1 y2)
  where
    range v1 v2
      | v1 == v2  = repeat v1
      | otherwise =
        let step = if v1 > v2 then -1 else 1
        in [v1,(v1 + step) .. v2]

decode :: MonadFail m => String -> m Line
decode l = case words l of
  [p1, "->", p2] -> (,) <$> decodePoint p1 <*> decodePoint p2
  _              -> failure
  where
    failure = fail $ "Bad line: " <> l
    decodePoint p = case break (== ',') p of
      (x, ',' : y) -> maybe failure pure $ (,) <$> readMaybe x <*> readMaybe y
      _            -> failure

selfCheck :: IO ()
selfCheck = do
  ls <- mapM decode
    [ "0,9 -> 5,9"
    , "8,0 -> 0,8"
    , "9,4 -> 3,4"
    , "2,2 -> 2,1"
    , "7,0 -> 7,4"
    , "6,4 -> 2,0"
    , "0,9 -> 2,9"
    , "3,4 -> 1,4"
    , "0,0 -> 8,8"
    , "5,5 -> 8,2"
    ]
  unless (solution1 ls == 5) $ fail "Step 1"
  unless (solution2 ls == 12) $ fail "Step 2"
