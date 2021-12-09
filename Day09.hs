{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import Control.Monad.State.Strict
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

newtype Depth = Depth Int deriving (Eq, Ord, Show)

type Pos = (Int, Int)
type Point = (Pos, Depth)
type Neibs = [(Point, [Point])]
type Floor = Map Pos (Depth, [Point])

main :: IO ()
main = do
  selfCheck
  i <- map decode . lines <$> readFile "Day09.input"
  print $ solution1 i
  print $ solution2 i

decode :: String -> [Depth]
decode = map $ Depth . read . (:[])

neibs :: [[Depth]] -> Neibs
neibs =
  concatMap (map repack . blocks) . enumerate . windows
  . surround (repeat edge) . map (surround edge)
  where
    edge = Depth (-1)
    repack (y, (     (_, u, _)
               , (x, (l, v, r))
               ,     (_, d, _))) =
      ( ((x, y), v)
      , [ ((x + dx, y + dy), n)
        | (dx, dy, n) <-
             [ ( 0,-1, u)
             , ( 0, 1, d)
             , (-1, 0, l)
             , ( 1, 0, r)
             ]
        , n /= edge
        ]
      )
    windows l = zip3 l (tail l) (tail $ tail l)
    blocks (y, (l1, l2, l3)) =
      map (y,) $ zip3 (windows l1) (enumerate $ windows l2) (windows l3)
    surround v vs = v : vs <> [v]
    enumerate = zip [0..]

solution1, solution2 :: [[Depth]] -> Int

solution1 = sum . map risk . lowPoints . neibs
  where
    risk (_, Depth v) = v + 1

lowPoints :: Neibs -> [Point]
lowPoints = map fst . filter isLowPoint
  where
    isLowPoint ((_, x), ys) = all ((> x) . snd) ys

solution2 = solve . neibs
  where
    solve ns =
      let ps = map fst $ lowPoints ns
          m = toMap ns
      in product . take 3 . reverse . sort $ map (basin m) ps

toMap :: Neibs -> Floor
toMap = foldl' step M.empty
  where
    step m ((pos, d), ns) = M.insert pos (d, ns) m

basin :: Floor -> Pos -> Int
basin m start = go start `evalState` S.empty
  where
    go pos = do
      s <- get
      if S.member pos s
        then pure 0
        else do
          modify $ S.insert pos
          let (_, ns) = (M.!) m pos
          vs <- mapM go [ p | (p, d) <- ns, d /= Depth 9 ]
          pure $ 1 + sum vs

selfCheck :: MonadFail m => m ()
selfCheck = do
  unless (solution1 example == 15) $ fail "solution1"
  unless (solution2 example == 1134) $ fail "solution1"

example :: [[Depth]]
example = map decode
  [ "2199943210"
  , "3987894921"
  , "9856789892"
  , "8767896789"
  , "9899965678"
  ]
