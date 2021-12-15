{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Control.Monad
import "astar" Data.Graph.AStar
import qualified "unordered-containers" Data.HashSet as HS
import Data.List
import Text.Read (readMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Pos = (Int, Int)
type Costs = Map Pos Int
type Input = (Costs, Int, Int)

main :: IO ()
main = do
  selfCheck
  Just i <- decode <$> readFile "Day15.input"
  print $ solution1 i
  print $ solution2 i

decode :: String -> Maybe Input
decode s = do
  rows@(r : _) <- pure $ lines s
  floorMap <- toFloor rows
  pure (floorMap, length r, length rows)
  where
    toFloor = fmap toMap . traverse (traverse (readMaybe . (:[])))
    toMap = foldl' insertRow Map.empty . zip [0..] . map (zip [0..])
    insertRow m (y, cs) = foldl' (insertCell y) m cs
    insertCell y m (x, v) = Map.insert (x, y) v m

solution1, solution2 :: Input -> Int
solution1 (costs, w, h) = searchAStar (costs Map.!) w h
solution2 (costs, w, h) = searchAStar cost (w * 5) (h * 5)
  where
    cost (x, y) =
      let (nx, xx) = divMod x w
          (ny, yy) = divMod y h
          c = costs Map.! (xx, yy) + nx + ny
      in if c <= 9 then c else c - 9

neibs :: Pos -> [Pos]
neibs (x, y) =
  [ (x - 1, y)
  , (x + 1, y)
  , (x, y - 1)
  , (x, y + 1)
  ]

searchAStar :: (Pos -> Int) -> Int -> Int -> Int
searchAStar cost w h =
  maybe (-1) (sum . map cost)
  $ aStar fork (const cost) (levi end) (== end) (0, 0)
  where
    end = (w - 1, h - 1)
    fork p = HS.fromList
      [ n
      | n@(x, y) <- neibs p
      , x >= 0 && x < w
      , y >= 0 && y < h
      ]

levi :: Pos -> Pos -> Int
levi (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

selfCheck :: MonadFail m => m ()
selfCheck = do
  Just snake <- pure . decode $ unlines
    [ "1111"
    , "9991"
    , "1111"
    , "1999"
    , "1111"
    ]
  unless (solution1 snake == 13) $ fail "bestWay"
  Just i <- pure $ decode example
  unless (solution1 i == 40) $ fail "solution1"
  unless (solution2 i == 315) $ fail "solution2"

example :: String
example = unlines
  [ "1163751742"
  , "1381373672"
  , "2136511328"
  , "3694931569"
  , "7463417111"
  , "1319128137"
  , "1359912421"
  , "3125421639"
  , "1293138521"
  , "2311944581"
  ]
