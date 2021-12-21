{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

module Main where

import Control.Monad
import Control.Monad.State.Strict
import Data.List
import Text.Read (readMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Input = (Int, Int)

main :: IO ()
main = do
  selfCheck
  Just i <- decode <$> readFile "Day21.input"
  print $ solution1 i
  print $ solution2 i

decode :: String -> Maybe Input
decode s = do
  [l1, l2] <- pure $ lines s
  p1 <- decodePlayer "1" l1
  p2 <- decodePlayer "2" l2
  pure (p1 - 1, p2 - 1)
  where
    decodePlayer n l = do
      ["Player", pn, "starting", "position:", pos] <- pure $ words l
      guard $ pn == n
      readMaybe pos

dice :: [Int]
dice = byThree throws
  where
    throws = [1..100] <> throws
    byThree (a:b:c:xs) = (a + b + c) : byThree xs
    byThree _ = [] -- impossible

play :: (Int, Int) -> (Int, Int)
play = uncurry $ go 1 dice 0 0
  where
    go t d sc1 sc2 p1 p2 =
      if sc1' >= 1000
      then (t * 3, sc2)
      else go (t + 1) d' sc2 sc1' p2 p1'
      where
        (throw : d') = d
        p1' = (p1 + throw) `mod` 10
        sc1' = sc1 + p1' + 1

explode :: Int -> Int -> Map (Int, Int, Int, Int) (Int, Int)
explode player1 player2 = go 0 0 player1 player2 `execState` Map.empty
  where
    dices = [ (d1, d2) | d1 <- [1..3], d2 <- [1..3] ]
    go s1 s2 p1 p2 = do
      Map.lookup k <$> get >>= \case
        Just v -> pure v
        Nothing -> do
          ts <- traverse (throw s1 s2 p1 p2) dices
          let p1n = sum $ map fst ts
          let p2n = sum $ map snd ts
          let v = (p1n, p2n)
          modify $ Map.insert k v
          pure v
      where
        k = (s1, s2, p1, p2)
    throw s1 s2 p1 p2 (t1, t2)
      | s1' >= 21 = pure (1, 0)
      | s2' >= 21 = pure (0, 1)
      | otherwise = go s1' s2' p1' p2'
      where
        p1' = (p1 + t1) `mod` 10
        p2' = (p2 + t2) `mod` 10
        s1' = s1 + p1' + 1
        s2' = s2 + p2' + 1

solution1, solution2 :: Input -> Int

solution1 = uncurry (*) . play
solution2 = const 0

selfCheck :: MonadFail m => m ()
selfCheck = do
  unless (solution1 example == 739785) $ fail "solution1"
  unless (solution2 example == 0) $ fail "solution2"
  where
    example = (3, 7) -- original (4, 8)
