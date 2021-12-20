{-# OPTIONS -Wall #-}

module Main where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Semigroup
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as Map

type Pos = (Int, Int)
type Algo = IntSet
type Image = Map Pos Bool
type Input = (Algo, Image)

main :: IO ()
main = do
  selfCheck
  i <- decode =<< readFile "Day20.input"
  print $ solution1 i  -- => 5503
  print $ solution2 i

decode :: MonadFail m => String -> m Input
decode raw = do
  (ra:_:is) <- pure $ lines raw
  a <- foldIM insertAlgo IntSet.empty ra
  i <- foldIM insertImgRow Map.empty is
  pure (a, i)
  where
    foldIM f a = foldM f a . zip [0..]
    insertAlgo s (i, c) = case c of
      '.' -> pure s
      '#' -> pure $ IntSet.insert i s
      _   -> fail $ "Bad char at " <> show i <> ": " <> [c]
    insertImgRow s (y, r) = foldIM (insertImgPxl y) s r
    insertImgPxl y s (x, c) = case c of
      '.' -> pure $ Map.insert (x, y) False s
      '#' -> pure $ Map.insert (x, y) True s
      _   -> fail $ "Bad char at " <> show (x, y) <> ": " <> [c]

apply :: Bool -> Algo -> Image -> Image
apply evenTurn alg img =
  let (rx, ry) = sizeOf img
  in Map.fromList
     [ ((x, y), measure evenTurn (x, y) img `IntSet.member` alg)
     | y <- range ry
     , x <- range rx
     ]

range :: (Min Int, Max Int) -> [Int]
range (Min start, Max stop) = [start - 1.. stop + 1]

measure :: Bool -> Pos -> Image -> Int
measure evenTurn (x, y) img = fromBits
  [ maybe evenTurn id $ Map.lookup (x + dx, y + dy) img
  | dy <- [-1 .. 1]
  , dx <- [-1 .. 1]
  ]

fromBits :: [Bool] -> Int
fromBits = sum . zipWith (\p x -> if x then p else 0) powers . reverse
  where
    powers = iterate (* 2) 1

sizeOf :: Image -> ((Min Int, Max Int), (Min Int, Max Int))
sizeOf = foldMap ((minMax *** minMax) . fst) . Map.toList
  where
    minMax = (,) <$> Min <*> Max

solution1, solution2 :: Input -> Int
solution1 = uncurry $ applyTimes 2
solution2 = uncurry $ applyTimes 50

applyTimes :: Int -> Algo -> Image -> Int
applyTimes n alg img =
  length . filter snd . Map.toList . foldl' step img
  $ take n (iterate not False)
  where
    step i f = apply f alg i

selfCheck :: IO ()
selfCheck = do
  example <- decode =<< readFile "Day20.example"
  unless (solution1 example == 31) $ fail "solution1"

display :: Image -> IO ()
display img =
  let (rx, ry) = sizeOf img
  in forM_ (range ry) $ \y -> do
    forM_ (range rx) $ \x ->
      putChar $ maybe ' ' (\c -> if c then '#' else '.') $ Map.lookup (x, y) img
    putChar '\n'
