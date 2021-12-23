{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -Wall #-}

{-
:set -package astar -package hashable -package unordered-containers
-}

module Main where

import Control.Monad
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Amphipod = A | B | C | D deriving (Eq, Ord, Show)

data Node
  = H1 | H2 | H3 | H4 | H5 | H6 | H7
  | R11 | R12 | R21 | R22 | R31 | R32 | R41 | R42
  deriving (Eq, Ord, Enum, Bounded, Show)

type Colony = Map Node Amphipod

type Costs = Map (Node, Node) Int

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

ways :: Map Node [(Int, Node)]
ways =
  foldl'
  (\m (n1, c, n2) ->
    Map.insertWith (<>) n1 [(c, n2)]
    $ Map.insertWith (<>) n2 [(c, n1)] m)
  Map.empty
  [ (H1, 1, H2)
  , (H2, 2, H3)
  , (H2, 2, R12)
  , (R12, 1, R11)
  , (H3, 2, H4)
  , (H3, 2, R22)
  , (R22, 1, R21)
  , (H4, 2, H5)
  , (H4, 2, R32)
  , (R32, 1, R31)
  , (H5, 2, H6)
  , (H5, 2, R42)
  , (R42, 1, R41)
  , (H6, 1, H7)
  ]

move :: Ord k => k -> k -> Map k v -> Maybe (Map k v)
move f t m = do
  v <- Map.lookup f m
  pure . Map.insert t v $ Map.delete f m

step :: Colony -> State Costs [Colony]
step c = do
  xs <-
    mapM store
    [ ((f, t), ) <$> tryStep c f t
    | f <- nodes
    , t <- nodes
    , f /= t
    ]
  pure $ catMaybes xs
  where
    store Nothing = pure Nothing
    store (Just (k, (v, x))) =
      Just x <$ modify (Map.insert (toKey k) v)

toKey :: (Node, Node) -> (Node, Node)
toKey (a, b) = (min a b, max a b)

tryStep :: Colony -> Node -> Node -> Maybe (Int, Colony)
tryStep cny f t = do
  a <- Map.lookup f cny
  guard $ a `canEnter` t
  v <- go Set.empty (0, f)
  (v ,) <$> move f t cny
  where
    go :: Set Node -> (Int, Node) -> Maybe Int
    go vs (s, x)
      | x == t          = Just s
      | Set.member x vs = Nothing
      | otherwise       =
        let xs = [ (s + s', n)
                 | Just ws <- [Map.lookup x ways]
                 , (s', n) <- ws
                 , not $ Map.member n cny
                 ]
            vs' = Set.insert x vs
        in case mapMaybe (go vs') xs of
          []    -> Nothing
          (y:_) -> pure y

canEnter :: Amphipod -> Node -> Bool
canEnter c n = (|| (n <= H7)) $ case c of
  A -> n == R11 || n == R12
  B -> n == R21 || n == R22
  C -> n == R31 || n == R32
  D -> n == R41 || n == R42

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
    [ fromA (getA =<< x)
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
    getA = Map.lookup `flip` m
    putRoom k = do
      putChar . fromA $ getA k
      putChar '#'

displayMoves :: [(Int, Colony)] -> IO ()
displayMoves = mapM_ $ \(c, m) -> do
  print c
  display m
