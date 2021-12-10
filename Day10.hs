{-# OPTIONS -Wall #-}

module Main where

import Control.Monad
import Data.List

data Line
  = Incomplete [Char]
  | Corrupted Char
  | Ok
  deriving (Show)

main :: IO ()
main = do
  selfCheck
  Just ls <- traverse toLine . lines <$> readFile "Day10.input"
  print $ solution1 ls
  print $ solution2 ls

decode :: String -> Maybe [Line]
decode = traverse toLine . lines

solution1, solution2 :: [Line] -> Int

solution1 xs = sum [ score x | Corrupted x <- xs ]
  where
    score :: Char -> Int
    score ')' = 3
    score ']' = 57
    score '}' = 1197
    score _   = 25137

solution2 xs = middle $ sort [ score x | Incomplete x <- xs ]
  where
    middle l = head $ drop (length l `div` 2) l
    score = foldl' step 0
    step s x = 5 * s + case x of
      ')' -> 1
      ']' -> 2
      '}' -> 3
      _   -> 4

toLine :: String -> Maybe Line
toLine = go []
  where
    go []  []     = Just Ok
    go acc []     = Just $ Incomplete acc
    go []  (x:xs) = do
      p <- pairFor x
      go [p] xs
    go acc@(a:as) (x:xs)
      | a == x    = go as xs
      | otherwise =
        case pairFor x of
          Just p  -> go (p:acc) xs
          Nothing -> Just $ Corrupted x

pairFor :: Char -> Maybe Char
pairFor '(' = Just ')'
pairFor '[' = Just ']'
pairFor '{' = Just '}'
pairFor '<' = Just '>'
pairFor _   = Nothing

selfCheck :: MonadFail m => m ()
selfCheck = do
  Just ls <- pure $ traverse toLine example
  unless (solution1 ls == 26397) $ fail "solution1"
  unless (solution2 ls == 288957) $ fail "solution1"

example :: [String]
example =
  [ "[({(<(())[]>[[{[]{<()<>>"
  , "[(()[<>])]({[<{<<[]>>("
  , "{([(<{}[<>[]}>{[]{[(<()>"
  , "(((({<>}<{<{<>}{[]{[]{}"
  , "[[<[([]))<([[{}[[()]]]"
  , "[{[{({}]{}}([{[{{{}}([]"
  , "{<[[]]>}<{[{[{[]{()[[[]"
  , "[<(<(<(<{}))><([]([]()"
  , "<{([([[(<>()){}]>(<<{{"
  , "<{([{{}}[<[[[<>{}]]]>[]]"
  ]
