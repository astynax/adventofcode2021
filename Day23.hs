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


data Amphipod = A | B | C | D deriving (Eq, Ord)
data Room
  = Empty
  | One !Amphipod
  | Two !Amphipod !Amphipod
  deriving (Eq, Ord)

type Space = Maybe Amphipod
data Colony = Colony
  { _h1 ::   !Space
  , _h2 ::   !Space
  , _r1 :: !Room
  , _h3 ::   !Space
  , _r2 :: !Room
  , _h4 ::   !Space
  , _r3 :: !Room
  , _h5 ::   !Space
  , _r4 :: !Room
  , _h6 ::   !Space
  , _h7 ::   !Space
  }
  deriving (Eq, Ord)

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

search :: Colony -> Maybe Int
search cny =
  Map.lookup gameOver $ fst $ go [(0, cny)] `execState` (Map.empty,  maxBound)
  where
    go [] = pure ()
    go xs = do
      vs <- forM xs $ \(t, c) -> do
        cache <- fst <$> get
        mt <- case Map.lookup c cache of
          Just pt
            | pt <= t   -> pure Nothing
            | otherwise -> pure $ Just pt
          Nothing       -> pure $ Just t
        case mt of
          Nothing -> pure []
          Just tt -> do
            modify $ \(m, n) ->
              let !m' = Map.insert c tt m
                  !n' = if c == gameOver then min tt n else n
              in (m', n')
            if c == gameOver
              then pure []
              else pure $ moves (tt, c)
      top <- snd <$> get
      go $ traceShow top $ concat vs

moves :: (Int, Colony) -> [(Int, Colony)]
moves (t, c) = [ (t + v, x) | (v, x) <- r2rs <> r2hs <> h2hs ]
  where
    r2hs = concat
      [ r2h c r1 [(2, h2), (1, h1)] [(2, h3), (2, h4), (2, h5), (2, h6), (1, h7)]
      , r2h c r2 [(2, h3), (2, h2), (1, h1)] [(2, h4), (2, h5), (2, h6), (1, h7)]
      , r2h c r3 [(2, h4), (2, h3), (2, h2), (1, h1)] [(2, h5), (2, h6), (1, h7)]
      , r2h c r4 [(2, h5), (2, h4), (2, h3), (2, h2), (1, h1)] [(2, h6), (1, h7)]
      ]
    r2rs = concat
      [ r2r c r1 [] [(h3, r2), (h4, r3), (h5, r4)]
      , r2r c r2 [(h3, r1)] [(h4, r3), (h5, r4)]
      , r2r c r3 [(h4, r2), (h3, r1)] [(h5, r4)]
      , r2r c r4 [(h5, r3), (h4, r2), (h3, r1)] []
      ]
    h2hs = concat
      [ h2h c h1 [] [(1, h2), (2, h3), (2, h4), (2, h5), (1, h6), (1, h7)]
      , h2h c h2 [(1, h1)] [(2, h3), (2, h4), (2, h5), (1, h6), (1, h7)]
      , h2h c h3 [(2, h2), (1, h1)] [(2, h4), (2, h5), (1, h6), (1, h7)]
      , h2h c h4 [(2, h3), (2, h2), (1, h1)] [(2, h5), (1, h6), (1, h7)]
      , h2h c h5 [(2, h4), (2, h3), (2, h2), (1, h1)] [(1, h6), (1, h7)]
      , h2h c h6 [(2, h5), (2, h4), (2, h3), (2, h2), (1, h1)] [(1, h7)]
      , h2h c h7 [(1, h6), (2, h5), (2, h4), (2, h3), (2, h2), (1, h1)] []
      ]

r2h
  :: Colony
  -> RoomL
  -> [(Int, SpaceL)]
  -> [(Int, SpaceL)]
  -> [(Int, Colony)]
r2h cny start ls rs = case cny ^. start of
  Empty   -> []
  One a   -> next (cny & start .~ Empty) a (cost a)
  Two b a -> next (cny & start .~ One b) a 0
  where
    next c a t = go c a t ls <> go c a t rs
    go _ _ _ [] = []
    go n a t ((c, l) : xs) = case n ^. l of
      Nothing -> (t', n & l .~ Just a) : go n a t' xs
      Just _  -> []
      where
        t' = t + cost a * c

r2r
  :: Colony
  -> RoomL
  -> [(SpaceL, RoomL)]
  -> [(SpaceL, RoomL)]
  -> [(Int, Colony)]
r2r cny start rs ls = case cny ^. start of
  Empty   -> []
  One a   -> next (cny & start .~ Empty) a (cost a * 2)
  Two b a -> next (cny & start .~ One b) a (cost a)
  where
    next c a t = go c a t ls <> go c a t rs
    go _ _ _ [] = [] -- impossible
    go n a t ((h, r) : xs) = case n ^. h of
      Just _  -> []
      Nothing ->
        let go' = go n a (t + cost a * 2) xs
        in case n ^. r of
          Two _ _ -> go'
          One b   -> (t + cost a * 3, n & r .~ Two b a) : go'
          Empty   -> (t + cost a * 4, n & r .~ One a) : go'

h2h
  :: Colony
  -> SpaceL
  -> [(Int, SpaceL)]
  -> [(Int, SpaceL)]
  -> [(Int, Colony)]
h2h cny start ls rs = case cny ^. start of
  Nothing -> []
  Just a  -> go a 0 ls <> go a 0 rs
  where
    go _ _ [] = []
    go a t ((c, l) : xs) = case cny ^. l of
      Nothing -> (t', new & l .~ Just a) : go a t' xs
      Just _  -> []
      where
        t' = t + cost a * c
        new = cny & start .~ Nothing

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
  = Colony
    Nothing
    Nothing
    (Two a1 b1)
    Nothing
    (Two a2 b2)
    Nothing
    (Two a3 b3)
    Nothing
    (Two a4 b4)
    Nothing
    Nothing

selfCheck :: MonadFail m => m ()
selfCheck = do
  unless (solution1 example == 0) $ fail "solution1"
  unless (solution2 example == 0) $ fail "solution2"

example :: Colony
example = colony A B D C C B A D

-- * Debug

display :: Colony -> IO ()
display Colony{..} = do
  putStrLn "#############"
  putChar '#'
  mapM_ (putChar . fromMA)
    [_h1, _h2, Nothing, _h3, Nothing, _h4, Nothing, _h5, Nothing, _h6, _h7]
  putStrLn "#"
  let rs = map fromRoom [_r1, _r2, _r3, _r4]
  putStr "###"
  mapM_ ((>> putChar '#') . putChar . snd) rs
  putStrLn "##"
  putStr "  #"
  mapM_ ((>> putChar '#') . putChar . fst) rs
  putStrLn ""
  putStrLn "  #########"
  where
    fromA A = 'A'
    fromA B = 'B'
    fromA C = 'C'
    fromA D = 'D'
    fromMA = maybe '.' fromA
    fromRoom Empty     = ('.',     '.')
    fromRoom (One x)   = (fromA x, '.')
    fromRoom (Two x y) = (fromA x, fromA y)

displayMoves :: [(Int, Colony)] -> IO ()
displayMoves = mapM_ $ \(c, m) -> do
  print c
  display m

-- * Poor-man optics

type Lens s a = ((s -> a), (s -> a -> s))
mkLens :: (s -> a) -> (s -> a -> s) -> Lens s a
mkLens = (,)

type SpaceL = Lens Colony Space
h1, h2, h3, h4, h5, h6, h7 :: SpaceL
h1 = mkLens _h1 $ \c v -> c{ _h1 = v }
h2 = mkLens _h2 $ \c v -> c{ _h2 = v }
h3 = mkLens _h3 $ \c v -> c{ _h3 = v }
h4 = mkLens _h4 $ \c v -> c{ _h4 = v }
h5 = mkLens _h5 $ \c v -> c{ _h5 = v }
h6 = mkLens _h6 $ \c v -> c{ _h6 = v }
h7 = mkLens _h7 $ \c v -> c{ _h7 = v }

type RoomL = Lens Colony Room
r1, r2, r3, r4 :: RoomL
r1 = mkLens _r1 $ \c v -> c{ _r1 = v }
r2 = mkLens _r2 $ \c v -> c{ _r2 = v }
r3 = mkLens _r3 $ \c v -> c{ _r3 = v }
r4 = mkLens _r4 $ \c v -> c{ _r4 = v }

{-# INLINE (.~) #-}
(.~) :: Lens s a -> a -> s -> s
(.~) (_, f) a s = f s a

{-# INLINE (^.) #-}
(^.) :: s -> Lens s a -> a
(^.) s (f, _) = f s
