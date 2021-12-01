{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List (tails)

main = do
  selfCheck
  xs <- map (read @Int) . lines <$> readFile "Day01.input"
  print $ stepsUp xs
  print $ stepsUp $ window 3 xs

stepsUp xs = length [ () | (x, y) <- zip xs (tail xs), x < y ]

window n = map (sum . take n) . init . tails

selfCheck =
  if stepsUp example == 7 && stepsUp (window 3 example) == 5
  then pure ()
  else fail "Self-check failed!"
  where
    example =
      [ 199
      , 200
      , 208
      , 210
      , 200
      , 207
      , 240
      , 269
      , 260
      , 263
      ]
