{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Data.Semigroup
import Text.Read (readMaybe)

type Point = (Int, Int)
type Area = ((Int, Int), (Int, Int))
type Throw = ((Int, Int), Int)
type Input = Area

main :: IO ()
main = do
  selfCheck
  i <- decode =<< readFile "Day17.input"
  let ts = successThrows i
  print $ solution1 ts
  print $ solution2 ts

decode :: MonadFail m => String -> m Input
decode s = do
  ["target", "area:", wx, wy] <- pure $ words s
  rx <- range $ init wx
  ry <- range wy
  pure (rx, ry)
  where
    range w = do
      (rb, '.':'.':re) <- pure . break (== '.') . drop 2 $ w
      Just b <- pure $ readMaybe rb
      Just e <- pure $ readMaybe re
      pure (b, e)

successThrows :: Area -> [Throw]
successThrows  a@((_, ax2), (ay1, _)) =
  [ ((dx, dy), my)
  | dx <- [0 .. ax2]
  , dy <- [ay1 .. negate ay1]
  , Just my <- [simulate a dx dy]
  ]

solution1, solution2 :: [Throw] -> Int

solution1 = maximum . map snd
solution2 = length

simulate :: Area -> Int -> Int -> Maybe Int
simulate area = go (Max 0) 0 0
  where
    go my x y dx dy
      | inside area x y = Just $ getMax my
      | missed area x y = Nothing
      | otherwise       =
        let (x', dx') = stepX x dx
            (y', dy') = stepY y dy
        in go (my <> Max y') x' y' dx' dy'

stepX :: Int -> Int -> (Int, Int)
stepX x dx
  | dx > 0    = next (dx - 1)
  | dx < 0    = next (dx + 1)
  | otherwise = next dx
  where
    next = (x + dx,)

stepY :: Int -> Int -> (Int, Int)
stepY y dy = (y + dy, dy - 1)

inside :: Area -> Int -> Int -> Bool
inside ((x1, x2), (y1, y2)) x y =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

missed :: Area -> Int -> Int -> Bool
missed ((_, x2), (y1, _)) x y = x > x2 || y < y1

selfCheck :: MonadFail m => m ()
selfCheck = do
  e <- decode "target area: x=20..30, y=-10..-5"
  unless (and [inside e 20 (-10), inside e 25 (-7)]) $ fail "inside"
  unless (and [missed e 31 (-8), missed e 25 (-11)]) $ fail "missed"
  unless (simulate e 6 9 == Just 45) $ fail "simulate"
  let ts = successThrows e
  unless (solution1 ts == 45) $ fail "solution1"
  unless (solution2 ts == 112) $ fail "solution2"
