{-# OPTIONS -Wall #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Monoid
import Text.ParserCombinators.ReadP

data Pair = Number !Int | Pair !Pair !Pair deriving (Eq, Show)
type Input = [Pair]

type History = [Either Pair Pair]
type Zipper = (Pair, History)

main :: IO ()
main = do
  selfCheck
  i <- decode =<< readFile "Day18.input"
  print $ solution1 i
  print $ solution2 i

-- * Decoding

decode :: MonadFail m => String -> m Input
decode = traverse decodeLine . lines
  where
    decodeLine l = case readP_to_S pairP l of
      [(x@(Pair _ _), [])] -> pure x
      _                    -> fail $ "Can't decode line: " <> l

pairP :: ReadP Pair
pairP = Pair
  <$> (char '[' *> elemP)
  <*> (char ',' *> elemP)
  <* char ']'

elemP :: ReadP Pair
elemP = Number <$> numP <|> pairP

numP :: ReadP Int
numP = read <$> many1 (satisfy isDigit)

-- * Zipper

start :: Pair -> Zipper
start = (, [])

top :: Zipper -> Zipper
top = tryHard up

up :: Zipper -> Maybe Zipper
up (p, hs) = case hs of
  []             -> Nothing
  (Right x : xs) -> Just (Pair p x, xs)
  (Left x  : xs) -> Just (Pair x p, xs)

left :: Zipper -> Maybe Zipper
left (Number _, _)  = Nothing
left (Pair l r, hs) = Just (l, Right r : hs)

right :: Zipper -> Maybe Zipper
right (Number _, _)  = Nothing
right (Pair l r, hs) = Just (r, Left l : hs)

prev :: Zipper -> Maybe Zipper
prev (_, []) = Nothing
prev (p, (Left l : hs)) = Just $ tryHard right (l, Right p : hs)
prev z = tryHard right <$> tryMoving up prev z

next :: Zipper -> Maybe Zipper
next (_, []) = Nothing
next (p, (Right r : hs)) = Just $ tryHard left (r, Left p : hs)
next z = tryHard left <$> tryMoving up next z

toNumbers :: Zipper -> [Int]
toNumbers = maybe [] id . go . tryHard left . top
  where
    go z = do
      (Number x, _) <- pure z
      (x :) <$> ((next z >>= go) <|> pure [])

modify :: (Pair -> Maybe Pair) -> Zipper -> Maybe Zipper
modify f (x, hs) = (, hs) <$> f x

walk :: Zipper -> [Zipper]
walk z@(p, _) = [z] <> case p of
  Number _ -> []
  _        -> walkTo left <> walkTo right
  where
    walkTo f = maybe [] id (walk <$> f z)

-- * Helpers

try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

tryHard :: (a -> Maybe a) -> a -> a
tryHard f x = maybe x (tryHard f) (f x)

tryHardBoth :: Eq a => (a -> Maybe a) -> (a -> Maybe a) -> a -> a
tryHardBoth f g x
  | x /= x'   = tryHardBoth f g x'
  | otherwise = x
  where
    x' = try g $ tryHard f x

tryMoving :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
tryMoving move f x = do
  x' <- move x
  f x' <|> tryMoving move f x'

-- * Actions

split :: Zipper -> Maybe Zipper
split (Number n, hs)
  | n > 9 =
    let l = n `div` 2
    in Just (Pair (Number l) (Number $ n - l), hs)
split _ = Nothing

explode :: Zipper -> Maybe Zipper
explode z@(Pair (Number l) (Number r), hs)
  | length hs == 4 =
    modify (const . Just $ Number 0)
    $ try (\x -> prev x >>= modify (add l) >>= next >>= up)
    $ try (\x -> next x >>= modify (add r) >>= prev >>= up)
    z
  where
    add v (Number x) = Just $ Number (x + v)
    add _ _          = Nothing
explode _ = Nothing

reduce :: Pair -> Pair
reduce = fst . tryHardBoth (tryAnywhere explode) (tryAnywhere split) . start

tryAnywhere :: (Zipper -> Maybe Zipper) -> Zipper -> Maybe Zipper
tryAnywhere f = fmap top . getFirst . foldMap (First . f) . walk

-- * Solutions

solution1, solution2 :: Input -> Int
solution1 = magnitude . addAll
solution2 ps = maximum
  [ magnitude (addPairs p1 p2)
  | (i, p1) <- ips
  , (j, p2) <- ips
  , i /= j
  ]
  where
    ips = zip [0 :: Int ..] ps

addPairs :: Pair -> Pair -> Pair
addPairs a b = reduce $ Pair a b

addAll :: [Pair] -> Pair
addAll [] = error "Nothing to add!"
addAll (x : xs) = foldl' addPairs x xs

magnitude :: Pair -> Int
magnitude (Number x) = x
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

-- * Self-testing

selfCheck :: MonadFail m => m ()
selfCheck = do
  [p] <- decode "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
  unless
    (display (reduce p)
     == "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
    $ fail "reduce"
  ex1 <- decode
    $ unlines
    [ "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
    , "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
    , "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
    , "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
    , "[7,[5,[[3,8],[1,4]]]]"
    , "[[2,[2,2]],[8,[8,1]]]"
    , "[2,9]"
    , "[1,[[[9,3],9],[[9,0],[0,7]]]]"
    , "[[[5,[7,4]],7],1]"
    , "[[[[4,2],2],6],[8,7]]"
    ]
  unless
    (display (addAll ex1)
     == "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
    $ fail "addAll"
  ex2 <- decode
    $ unlines
    [ "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
    , "[[[5,[2,8]],4],[5,[[9,9],0]]]"
    , "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
    , "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
    , "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
    , "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
    , "[[[[5,4],[7,7]],8],[[8,3],8]]"
    , "[[9,3],[[9,9],[6,[4,9]]]]"
    , "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
    , "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
    ]
  unless (solution1 ex2 == 4140) $ fail "Oops!"

display :: Pair -> String
display (Number x) = show x
display (Pair l r) = "[" <> display l <> "," <> display r <> "]"
