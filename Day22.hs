{-# OPTIONS -Wall #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Debug.Trace
import Control.Monad
import Data.List
import Text.Read (readMaybe)
import qualified Data.Set as Set

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

isNear :: Cuboid -> Bool
isNear (rx, ry, rz) = near rx && near ry && near rz
  where
    near (b, e) = b >= -50 && e <= 50

solution2 = applyAll2

applyAll :: Input -> Int
applyAll = Set.size . go Set.empty
  where
    go !s [] = s
    go !s ((f, (rx, ry, rz)) : xs) = go s' xs
      where
        s' = (if f then Set.union else Set.difference) s c
        c = Set.fromList
          [ (x, y, z)
          | x <- range rx
          , y <- range ry
          , z <- range rz
          ]

{-# INLINE range #-}
range :: (Int, Int) -> [Int]
range = uncurry enumFromTo

{-# INLINE getRX #-}
getRX :: Cuboid -> Range
getRX (rx, _, _) = rx

{-# INLINE getRY #-}
getRY (_, rx, _) = rx
getRY :: Cuboid -> Range

{-# INLINE getRZ #-}
getRZ :: Cuboid -> Range
getRZ (_, _, rx) = rx

applyAll2 :: Input -> Int
applyAll2 i = length
  [ ()
  | let
  , (traceShowId -> x) <-range $ traceShowId $ measureBy getRX is
  , let is' = filter ((`containsR` x) . getRX . snd) is
  , y <- range $ measureBy getRY is'
  , let is'' = filter ((`containsR` y) . getRY . snd) is'
  , z <- range $ measureBy getRZ is''
  , test (x, y, z) is''
  ]
  where
    is = reverse i
    test p xs = case filter ((`containsC` p) . snd) xs of
      ((v, _):_) -> v
      _          -> False

measureBy :: (Cuboid -> Range) -> Input -> Range
measureBy getR = foldl' step (0, 0)
  where
    step r = merge r . getR . snd

{-# INLINE merge #-}
merge :: Range -> Range -> Range
merge (b1, e1) (b2, e2) = (min b1 b2, max e1 e2)

{-# INLINE containsC #-}
containsC :: Cuboid -> Pos -> Bool
containsC (rx, ry, rz) (x, y, z) =
     containsR rx x
  && containsR ry y
  && containsR rz z

{-# INLINE containsR #-}
containsR :: Range -> Int -> Bool
containsR (b, e) x = b <= x && x <= e

selfCheck :: IO ()
selfCheck = do
  Just i1 <- decode <$> readFile "Day22.example1"
  unless (solution1 i1 == 590784) $ fail "solution1"
  Just i2 <- decode <$> readFile "Day22.example2"
  unless (solution2 i2 == 0) $ fail "solution2"
