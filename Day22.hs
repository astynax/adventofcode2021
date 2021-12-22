{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Maybe (mapMaybe)
import Control.Monad
import Data.List
import Text.Read (readMaybe)

type Pos = (Int, Int, Int)
type Range = (Int, Int)
type Cuboid = (Range, Range, Range)
type Input = [(Bool, Cuboid)]

main :: IO ()
main = do
  -- selfCheck
  Just i <- decode <$> readFile "Day22.input"
  print $ solution1 i
  print $ solution2 i

decode :: String -> Maybe Input
decode = traverse fromLine . lines
  where
    fromLine l = do
      [rf, rxyz] <- pure $ words l
      f <- case rf of
        "on"  -> pure True
        "off" -> pure False
        _     -> fail "Unexpected state"
      (rx, _:ryz) <- pure $ break (== ',') rxyz
      (ry, _:rz)  <- pure $ break (== ',') ryz
      pos <- (,,) <$> fromRange rx <*> fromRange ry <*> fromRange rz
      pure (f, pos)
    fromRange s = do
      (_:_:rest) <- pure s
      (rb, _:_:re) <- pure $ break (== '.') rest
      r <- (,) <$> readMaybe rb <*> readMaybe re
      guard $ uncurry (<=) r
      pure r

solution1, solution2 :: Input -> Int
solution1 = applyAll . filter (isNear . snd)
solution2 = applyAll

isNear :: Cuboid -> Bool
isNear (rx, ry, rz) = near rx && near ry && near rz
  where
    near (b, e) = b >= -50 && e <= 50

-- Inspired by: https://jhidding.github.io/aoc2021/#day-22-reactor-reboot
applyAll :: Input -> Int
applyAll = total . foldl' step []
  where
    total = sum . map (\(c, n) -> volume c * n)
    step cs (s, c)
      | s         = enable c cs
      | otherwise = disable c cs

enable, disable :: Cuboid -> [(Cuboid, Int)] -> [(Cuboid, Int)]
enable c cs = (c, 1) : disable c cs
disable c cs = mapMaybe chunk cs <> cs
  where
    chunk (x, n) = (, negate n) <$> intersectC x c

volume :: Cuboid -> Int
volume ((x1, x2), (y1, y2), (z1, z2)) =
  (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)

intersectC :: Cuboid -> Cuboid -> Maybe Cuboid
intersectC (rx1, ry1, rz1) (rx2, ry2, rz2) = (,,)
  <$> intersectR rx1 rx2
  <*> intersectR ry1 ry2
  <*> intersectR rz1 rz2

intersectR :: Range -> Range -> Maybe Range
intersectR (b1, e1) (b2, e2)
  | e1 < b2 || b1 > e2 = Nothing
  | otherwise          = Just (max b1 b2, min e1 e2)

selfCheck :: IO ()
selfCheck = do
  Just i1 <- decode <$> readFile "Day22.example1"
  unless (solution1 i1 == 590784) $ fail "solution1"
  Just i2 <- decode <$> readFile "Day22.example2"
  unless (solution2 i2 == 2758514936282235) $ fail "solution2"
