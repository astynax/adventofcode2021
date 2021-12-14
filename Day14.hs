{-# OPTIONS -Wall #-}

module Main where

import Control.Monad
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Rules = Map (Char, Char) Char
type Input = (String, Rules)

main :: IO ()
main = do
  selfCheck
  Just i <- decode <$> readFile "Day14.input"
  print $ solution1 i
  print $ solution2 i

decode :: String -> Maybe Input
decode s = do
  ([pat], "" : rs) <- pure . break null $ lines s
  rules <- traverse toRule rs
  pure (pat, toMap rules)
  where
    toRule l = case words l of
      [[c1, c2], "->", [c3]] -> Just ((c1, c2), c3)
      _                      -> Nothing
    toMap = foldl' (\m (k, v) -> Map.insert k v m) Map.empty

solution1, solution2 :: Input -> Maybe Int
solution1 = solveFor 10
-- solution2 = solveFor 40 -- No way!
solution2 = const (Just 0)

solveFor :: Int -> Input -> Maybe Int
solveFor n (p, m) = do
  fs@(lfc : _) <- pure . map snd . frequences $ steps n m p
  [mfc] <- pure . take 1 $ reverse fs
  pure $ mfc - lfc

step :: Rules -> String -> String
step m s = concatMap replace $ zip s (tail s <> "?")
  where
    replace k@(f, _) = case Map.lookup k m of
      Just c  -> [f, c]
      Nothing -> [f]

steps :: Int -> Rules -> String -> String
steps n m = head . drop n . iterate (step m)

frequences :: String -> [(Char, Int)]
frequences = sortOn snd . Map.toList . foldl' count Map.empty
  where
    count m c = Map.insertWith (+) c 1 m

selfCheck :: MonadFail m => m ()
selfCheck = do
  Just i@(p, m) <- pure $ decode example
  unless (steps 4 m p == "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")
    $ fail "step"
  unless (solution1 i == Just 1588) $ fail "solution1"
  -- unless (solution2 i == Just 2188189693529) $ fail "solution2"

example :: String
example = unlines
  [ "NNCB"
  , ""
  , "CH -> B"
  , "HH -> N"
  , "CB -> H"
  , "NH -> C"
  , "HB -> C"
  , "HC -> B"
  , "HN -> C"
  , "NN -> C"
  , "BH -> H"
  , "NC -> B"
  , "NB -> B"
  , "BN -> B"
  , "BB -> N"
  , "BC -> B"
  , "CC -> N"
  , "CN -> C"
  ]
