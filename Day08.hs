module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Text.Read

type Wiring = (Char, Char, Char, Char, Char, Char, Char)

main = do
  selfCheck
  i <- mapM decode . lines =<< readFile "Day08.input"
  print $ solution1 i
  print $ solution2 i

decode :: MonadFail m => String -> m ([String], [String])
decode s = case break (== '|') s of
  (v, _ : d) -> pure (words v, words d)
  _          -> fail $ "Bad input: " <> s

solution1, solution2 :: [([String], [String])] -> Int

solution1 = sum . map (length . filter good . snd)
  where
    good s = let i = length s in i == 2 || i == 3 || i == 4 || i == 7

solution2 = maybe (error "Oops!") sum . traverse guessNumber

guess :: [String] -> Maybe Wiring
guess xs = do
  one   <- find (long 2) xs
  seven <- find (long 3) xs
  four  <- find (long 4) xs
  eight <- find (long 7) xs

  cde@[_, _, _] <- pure [ x | [x] <- map (diff eight) xs ]

  [a] <- pure [ x | x <- seven, x `notIn` one ]
  [e] <- pure [ x | x <- cde, x `notIn` four, x `notIn` one ]
  [d] <- pure [ x | x <- cde, x /= e, x `notIn` one ]
  [c] <- pure [ x | x <- cde, x `notIn` [d, e] ]
  [g] <- pure [ x | n <- filter (long 5) xs, [x] <- [diff n [a, c, d, e]] ]

  bf@[_, _] <- pure [ x | x <- eight, x `notIn` [a, c, d, e, g] ]

  [b] <- pure [ x | x <- bf, x `notIn` seven ]
  [f] <- pure [ x | x <- bf, x /= b ]

  pure (a, b, c, d, e, f, g)
  where
    long n = (== n) . length
    notIn c = not . (c `elem`)
    diff as bs = [ a | a <- as, not (a `elem` bs) ]

digits :: Wiring -> [(String, Int)]
digits (a, b, c, d, e, f, g) =
  flip zip [0..] $ map sort
  [ [a, b, c, e, f, g]
  , [c, f]
  , [a, c, d, e, g]
  , [a, c, d, f, g]
  , [b, c, d, f]
  , [a, b, d, f, g]
  , [a, b, d, e, f, g]
  , [a, c, f]
  , [a, b, c, d, e, f, g]
  , [a, b, c, d, f, g]
  ]

guessNumber :: ([String], [String]) -> Maybe Int
guessNumber (ns, ds) = do
  w <- guess ns
  let m = digits w
  let toDigit = (`lookup` m) . sort
  gs <- traverse toDigit ds
  readMaybe $ concatMap show gs

selfCheck :: IO ()
selfCheck = do
  xs@(x : _) <- mapM decode example
  unless (solution1 xs == 26) $ fail "solution1"
  unless (isJust $ traverse (guess . fst) xs) $ fail "guess"
  unless (guessNumber x == Just 8394) $ fail "guessNumber"
  unless (solution2 xs == 61229) $ fail "solution2"
  where
    example =
      [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
      , "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
      , "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
      , "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
      , "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
      , "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
      , "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
      , "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
      , "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
      , "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
      ]
