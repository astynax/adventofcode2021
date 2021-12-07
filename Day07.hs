module Main where

import Control.Monad
import Data.List
import Text.Read

main = do
  selfCheck
  i <- decode =<< readFile "Day07.input"
  print $ solution1 i
  print $ solution2 i

decode :: MonadFail m => String -> m [Int]
decode s = maybe (fail "Bad input") pure . readMaybe $ "[" <> s <> "]"

solution1, solution2 :: [Int] -> Int

solution1 = costBy abs
solution2 = costBy (\i -> sum [1 .. abs i])

costBy cost xs = minimum $ map totalCost positions
  where
    positions = [minimum xs .. maximum xs]
    totalCost p = sum [ cost (x - p) | x <- xs ]

selfCheck :: MonadFail m => m ()
selfCheck = do
  unless (solution1 example == 37) $ fail "Step 1"
  unless (solution2 example == 168) $ fail "Step 2"
  where
    example = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]
