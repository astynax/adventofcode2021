{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

{-
:set -package astar -package hashable -package unordered-containers
-}

module Main where

import Debug.Trace
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Function ((&))
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Amphipod = A | B | C | D deriving (Eq)

data Node
  = H1 | H2 | H3 | H4 | H5 | H6 | H7
  | R11 | R12 | R21 | R22 | R31 | R32 | R41 | R42
  deriving (Eq, Ord, Enum, Bounded)

type Colony = Map Node Amphipod

nodes :: [Node]
nodes = [minBound .. maxBound]

type Input = Colony

gameOver :: Colony
gameOver = colony
  A A
  B B
  C C
  D D

main :: IO ()
main = do
  selfCheck
  Just i <- decode <$> readFile "Day21.input"
  print $ solution1 i
  print $ solution2 i

decode :: String -> Maybe Input
decode = undefined

solution1, solution2 :: Input -> Int

solution1 = const 0

solution2 = const 0

ways :: Map (Node, Node) (Colony -> Maybe (Int, Colony))
ways = go `execState` Map.empty
  where
    go = undefined

cost :: Amphipod -> Int
cost A = 1
cost B = 10
cost C = 100
cost D = 1000

colony
  :: Amphipod -> Amphipod
  -> Amphipod -> Amphipod
  -> Amphipod -> Amphipod
  -> Amphipod -> Amphipod
  -> Colony
colony
  a1 b1
  a2 b2
  a3 b3
  a4 b4
  = Map.fromList
    [ (R11, a1), (R12, b1)
    , (R21, a2), (R22, b2)
    , (R31, a3), (R32, b3)
    , (R41, a4), (R42, b4)
    ]

selfCheck :: MonadFail m => m ()
selfCheck = do
  unless (solution1 example == 0) $ fail "solution1"
  unless (solution2 example == 0) $ fail "solution2"

example :: Colony
example = colony A B D C C B A D

-- * Debug

display :: Colony -> IO ()
display m = do
  putStrLn "#############"
  putChar '#'
  mapM_ putChar
    [ fromA (get =<< x)
    | x <- [ Just H1, Just H2, Nothing, Just H3, Nothing
           , Just H4, Nothing, Just H5, Nothing, Just H6, Just H7 ] ]
  putStrLn "#"
  putStr "###"
  mapM_ putRoom [R12, R22, R32, R42]
  putStrLn "##"
  putStr "  #"
  mapM_ putRoom [R11, R21, R31, R41]
  putStrLn ""
  putStrLn "  #########"
  where
    fromA (Just A) = 'A'
    fromA (Just B) = 'B'
    fromA (Just C) = 'C'
    fromA (Just D) = 'D'
    fromA _        = '.'
    get = Map.lookup `flip` m
    putRoom k = do
      putChar . fromA $ get k
      putChar '#'

displayMoves :: [(Int, Colony)] -> IO ()
displayMoves = mapM_ $ \(c, m) -> do
  print c
  display m
