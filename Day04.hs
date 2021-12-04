module Main where

import Control.Monad
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.List

data Board = Board ![IntSet] ![IntSet] deriving Show

main = do
  selfCheck
  (seq, bs) <- decode . lines <$> readFile "Day04.input"
  print $ solution1 seq bs
  print $ solution2 seq bs

-- loading

decode :: [String] -> ([Int], [Board])
decode (s:_:bs) = (seq, boards)
  where
    seq = read $ "[" <> s <> "]"  -- Yep :)
    boards = map toBoard $ splitByNull bs
    toBoard = fromLists . map (map read . words)

splitByNull :: [[a]] -> [[[a]]]
splitByNull = unfoldr $ \l ->
  case break null l of
    ([], _) -> Nothing
    (x, xs) -> Just (x, drop 1 xs)

fromLists :: [[Int]] -> Board
fromLists = Board <$> rs <*> cs
  where
    rs = map S.fromList
    cs xs = zipWith (\_ n -> S.fromList $ map (!! n) xs) xs [0..]

-- solving

solution1 :: [Int] -> [Board] -> Int
solution1 []     _  = error "Impossible"
solution1 (n:ns) bs =
  let bs' = map (apply n) bs
  in case find isWinning bs' of
    Just b  -> score n b
    Nothing -> solution1 ns bs'

solution2 :: [Int] -> [Board] -> Int
solution2 []     _  = error "Impossible"
solution2 (n:ns) bs =
  let bs' = map (apply n) bs
  in case filter (not . isWinning) bs' of
    []  -> error "Impossible"
    [x] -> fillLast ns x
    xs  -> solution2 ns bs'
  where
    fillLast []     _ = error "Impossible"
    fillLast (i:is) b =
      case apply i b of
        x | isWinning x -> score i x
          | otherwise   -> fillLast is x

isWinning :: Board -> Bool
isWinning (Board rs cs) = any (== S.empty) rs || any (== S.empty) cs

apply :: Int -> Board -> Board
apply i (Board rs cs) = Board (map (S.delete i) rs) (map (S.delete i) cs)

score :: Int -> Board -> Int
score l (Board rs _) = l * sum (concatMap S.toList rs)

exampleBoard = fromLists
  [ [14, 21, 17, 24,  4]
  , [10, 16, 15,  9, 19]
  , [18,  8, 23, 26, 20]
  , [22, 11, 13,  6,  5]
  , [ 2,  0, 12,  3,  7]
  ]

exampleSeq = [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24]

selfCheck :: IO ()
selfCheck = do
  let b = foldr apply exampleBoard exampleSeq
  unless (isWinning b) $ fail "isWinning"
  unless (score (last exampleSeq) b == 4512) $ fail "score"
