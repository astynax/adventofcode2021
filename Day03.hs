module Main where

import Control.Monad (unless)
import Data.List (foldl')

main = do
  selfCheck
  bytes <- lines <$> readFile "Day03.input"
  print $ solution1 bytes
  print $ solution2 bytes

solution1 xs =
  let g = gamma xs
      d = complement g
  in fromBits g * fromBits d

solution2 xs =
  let o2  = filterBytes (==) xs
      co2 = filterBytes (/=) xs
  in fromBits o2 * fromBits co2

gamma :: [String] -> String
gamma [] = ""
gamma xs@(x:_) = toBits $ foldl' step zeroes xs
  where
    zeroes = map (const 0) x
    step = zipWith stepBit
    stepBit v b = if b == '1' then v + 1 else v - 1
    toBits = map $ \x -> if x < 0 then '0' else '1'

complement = map $ \x -> if x == '1' then '0' else '1'

fromBits :: String -> Int
fromBits xs =
  sum $ zipWith (\x p -> if x == '1' then round (2 ** p) else 0)
  (reverse xs)
  [0..]

countBitsAt :: Int -> [String] -> Char
countBitsAt n xs =
  let v = foldl' step 0 $ map (!! n) xs
  in if v < 0 then '0' else '1'
  where
    step v b = if b == '1' then v + 1 else v - 1

filterBytes :: (Char -> Char -> Bool) -> [String] -> String
filterBytes f = go 0
  where
    go _ []  = error "Impossible!"
    go _ [x] = x
    go n xs
      | n > length (head xs) = error "Impossible!"
      | otherwise =
        let bit = countBitsAt n xs
        in go (n + 1) $ filter (f bit . (!! n)) xs

selfCheck :: IO ()
selfCheck = do
  unless (solution1 example == 198) $ error "Step 1"
  unless (solution2 example == 230) $ error "Step 2"

example =
  [ "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ]
