{-# OPTIONS -Wall #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Monad
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Cave
  = Start
  | End
  | Large !String
  | Small !String
  deriving (Ord, Eq, Show)

type CaveMap = Map Cave [Cave]

main :: IO ()
main = do
  selfCheck
  Just i <- decode <$> readFile "Day12.input"
  print $ solution1 i
  print $ solution2 i

decode :: String -> Maybe CaveMap
decode =
  fmap (Map.map Set.toList . foldl' step Map.empty)
  . traverse decodeLine . lines
  where
    decodeLine l = case break (== '-') l of
      (f, _:t) -> (,) <$> toCave f <*> toCave t
      _        -> Nothing
    toCave = \case
      "start"           -> Just Start
      "end"             -> Just End
      x | all isLower x -> Just $ Small x
        | all isUpper x -> Just $ Large x
      _                 -> Nothing
    step m (f, t) = putWay f t . putWay t f $ m
    putWay f t = Map.insertWith Set.union f (Set.singleton t)

solution1, solution2 :: CaveMap -> Int

solution1 = length . paths False

paths :: Bool -> CaveMap -> [[Cave]]
paths revizitable caveMap = pathsFrom revizitable Set.empty Start
  where
    pathsFrom _ _ End = [[End]]
    pathsFrom canRevizit visited cave
      | cave `Set.member` visited =
        if | cave == Start -> []
           | canRevizit    -> continue False
           | otherwise     -> []
      | otherwise              =
        continue canRevizit
      where
        continue reviziting = concatMap (map (cave :) . fork reviziting) ways
        ways = caveMap Map.! cave
        fork = pathsFrom `flip` case cave of
          Large _ -> visited
          _       -> Set.insert cave visited

solution2 = length . paths True

selfCheck :: MonadFail m => m ()
selfCheck = do
  Just i <- pure $ decode example
  unless (solution1 i == 10) $ fail "solution1"
  unless (solution2 i == 36) $ fail "solution2"

example :: String
example = unlines
  [ "start-A"
  , "start-b"
  , "A-c"
  , "A-b"
  , "b-d"
  , "A-end"
  , "b-end"
  ]

display :: [[Cave]] -> IO ()
display = mapM_ $ putStrLn . intercalate "-" . map fromCave
  where
    fromCave = \case
      Start   -> "start"
      End     -> "end"
      Large s -> s
      Small s -> s
