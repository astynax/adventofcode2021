{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -Wall #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Bool (bool)
import Debug.Trace
import Text.Read (readMaybe)

data Var = X | Y | Z | W deriving (Eq, Show)
data Arg = Var Var | Num Int deriving (Eq, Show)

data Op
  = Inp Var
  | Add Var Arg
  | Mul Var Arg
  | Div Var Arg
  | Mod Var Arg
  | Eql Var Arg
  deriving (Eq, Show)

data Alu a = Alu
  { aluX :: !a
  , aluY :: !a
  , aluZ :: !a
  , aluW :: !a
  } deriving (Eq, Show)

type Input = [Op]

main :: IO ()
main = do
  selfCheck
  Just i <- decode <$> readFile "Day24.input"
  print $ solution1 i
  print $ solution2 i

decode :: String -> Maybe Input
decode = traverse opD . lines

opD :: String -> Maybe Op
opD s = case words s of
  ["inp", v]    -> Inp <$> varD v
  ["add", v, a] -> Add <$> varD v <*> argD a
  ["mul", v, a] -> Mul <$> varD v <*> argD a
  ["div", v, a] -> Div <$> varD v <*> argD a
  ["mod", v, a] -> Mod <$> varD v <*> argD a
  ["eql", v, a] -> Eql <$> varD v <*> argD a
  _             -> fail $ "Bad op: " <> s
  where
    argD x = Num <$> readMaybe x <|> Var <$> varD x
    varD = \case
      "x" -> pure X
      "y" -> pure Y
      "z" -> pure Z
      "w" -> pure W
      x   -> fail $ "Bad var name: " <> x

solution1, solution2 :: Input -> Maybe Int
solution1 = const (Just 0)
solution2 = const (Just 0)

searchSN :: [Op] -> [Int]
searchSN = map fromDigits . go (Alu 0 0 0 0)
  where
    go alu [] = if aluZ alu == 0 then [[]] else []
    go alu (o:os) = case step [] o alu of
      Nothing -> case o of
        Inp v -> do
          i <- [1 .. 9]
          (i:) <$> go (writeVar v (traceShowId i) alu) os
        _ -> error $ "Failed step for " <> show o
      Just (_, alu') ->
        go alu' os

isValidSN :: [Op] -> Int -> Bool
isValidSN pgm num =
  let ds = digits num
  in not (0 `elem` ds) && aluZ (run ds pgm) == 0

digits :: Int -> [Int]
digits 0 = [0]
digits x = reverse $ go x
  where
    go 0 = []
    go n = let (d, m) = divMod n 10 in m : go d

fromDigits :: [Int] -> Int
fromDigits []     = 0
fromDigits (x:xs) = foldl' (\a b -> a * 10 + b) x xs

run :: [Int] -> [Op] -> Alu Int
run i =
  snd . foldl'
  (\(is, alu) o -> maybe (error "Input underflow!") id $ step is o alu)
  (i, Alu 0 0 0 0)

step :: [Int] -> Op -> Alu Int -> Maybe ([Int], Alu Int)
step input = \case
  Inp v -> case input of
    []     -> const Nothing
    (i:is) -> Just . (is, ) . writeVar v i
  Add v a  -> next v a (+)
  Mul v a  -> next v a (*)
  Div v a  -> next v a div
  Mod v a  -> next v a mod
  Eql v a  -> next v a $ (bool 0 1 .) . (==)
  where
    next v a f = Just . (input,) . update v a (\x -> f x . either id id)

readVar :: Var -> Alu a -> a
readVar = \case
  X -> aluX
  Y -> aluY
  Z -> aluZ
  W -> aluW

writeVar :: Var -> a -> Alu a -> Alu a
writeVar = \case
  X -> \v alu -> alu { aluX = v }
  Y -> \v alu -> alu { aluY = v }
  Z -> \v alu -> alu { aluZ = v }
  W -> \v alu -> alu { aluW = v }

update :: Var -> Arg -> (a -> Either a Int -> a) -> Alu a -> Alu a
update var arg f alu =
  let a = readVar var alu
      b = case arg of
        Var v -> Left $ readVar v alu
        Num i -> Right i
  in writeVar var (f a b) alu

selfCheck :: IO ()
selfCheck = do
  raw <- readFile "Day24.input"
  assertEq "roundtrip"
    (Just raw)
    $ unlines . map display <$> decode raw

  assertEq "int2bin"
    (Just $ Alu 0 1 0 1)
    $ run [10] <$> decode exInt2Bin

  assertEq "solution1" (Just 0) $ solution1 undefined
  assertEq "solution2" (Just 0) $ solution2 undefined
  where
    assertEq name y x =
      unless (x == y) $ fail
      $ "[" <> name <> "]: " <> show x <> " /= " <> show y
    exInt2Bin = unlines
      [ "inp w"
      , "add z w"
      , "mod z 2"
      , "div w 2"
      , "add y w"
      , "mod y 2"
      , "div w 2"
      , "add x w"
      , "mod x 2"
      , "div w 2"
      , "mod w 2"
      ]

display :: Op -> String
display = unwords . opDisp
  where
    opDisp = \case
      Inp v   -> ["inp", varDisp v]
      Add v a -> ["add", varDisp v, argDisp a]
      Mul v a -> ["mul", varDisp v, argDisp a]
      Div v a -> ["div", varDisp v, argDisp a]
      Mod v a -> ["mod", varDisp v, argDisp a]
      Eql v a -> ["eql", varDisp v, argDisp a]
    varDisp = \case
      X -> "x"
      Y -> "y"
      Z -> "z"
      W -> "w"
    argDisp = \case
      Var v -> varDisp v
      Num i -> show i
