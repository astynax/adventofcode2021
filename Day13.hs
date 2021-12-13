{-# OPTIONS -Wall #-}

module Main where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.List
import Text.Read
import Data.Set (Set)
import qualified Data.Set as Set

type Paper = Set (Int, Int)
data Axis = X | Y deriving (Show, Read)
type Fold = (Axis, Int)
type Input = (Paper, [Fold])

main :: IO ()
main = do
  selfCheck
  Just i <- decode <$> readFile "Day13.input"
  print $ solution1 i
  solution2 i

decode :: String -> Maybe Input
decode s = do
  (ps, "" : fs) <- pure $ break null $ lines s
  paper <- Set.fromList <$> traverse toPair ps
  folds <- traverse toFold fs
  pure (paper, folds)
  where
    toPair l = readMaybe $ "(" <> l <> ")"
    toFold l = do
      [_, _, f] <- pure $ words l
      (sa, '=' : sv) <- pure $ break (== '=') f
      a <- readMaybe $ map toUpper sa
      v <- readMaybe sv
      pure (a, v)

solution1 :: Input -> Int
solution1 (p, fs) = length $ foldl' (flip fold) p $ take 1 fs

solution2 :: Input -> IO ()
solution2 (p, fs) = display $ foldl' (flip fold) p fs

fold :: Fold -> Paper -> Paper
fold (axis, pos) = Set.map $ mapTo axis transform
  where
    mapTo X = first
    mapTo Y = second
    transform v = if v < pos then v else pos - (v - pos)

selfCheck :: MonadFail m => m ()
selfCheck = do
  Just i <- pure $ decode example
  unless (solution1 i == 17) $ fail "solution1"

example :: String
example = unlines
  [ "6,10"
  , "0,14"
  , "9,10"
  , "0,3"
  , "10,4"
  , "4,11"
  , "6,0"
  , "6,12"
  , "4,1"
  , "0,13"
  , "10,12"
  , "3,4"
  , "3,0"
  , "8,4"
  , "1,10"
  , "2,14"
  , "8,10"
  , "9,0"
  , ""
  , "fold along y=7"
  , "fold along x=5"
  ]

display :: Paper -> IO ()
display p = forM_ ys $ \y -> do
  forM_ xs $ \x -> putChar $ if (x, y) `Set.member` p then '#' else '.'
  putChar '\n'
  where
    ps = Set.toList p
    ys = range snd
    xs = range fst
    range f =  enumFromTo <$> minimum <*> maximum $ map f ps
