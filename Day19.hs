{-# OPTIONS -Wall #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.List
import Text.Read (readMaybe)

type Point = (Int, Int, Int)
type Scanner = [Point]
type Input = (Scanner, [Scanner])

main :: IO ()
main = do
  selfCheck
  i <- decode =<< readFile "Day19.input"
  Just ps <- pure $ syncAll i
  print $ solution1 ps
  print $ solution2 ps

decode :: MonadFail m => String -> m Input
decode i = do
  (s:ss) <- go $ lines i
  pure (s, ss)
  where
    go [] = pure []
    go s = do
      (_:rbs, rest) <- pure $ break null s
      Just bs <- pure . traverse readMaybe $ map (("(" <>) . (<> ")")) rbs
      (bs :) <$> go (drop 1 rest)

rebaseTo :: Point -> [Point] -> ((Int, Int, Int), [Point])
rebaseTo _ [] = error "FIXME: impossible!"
rebaseTo (x0, y0, z0) ps@((x1, y1, z1) : _) =
  ( (dx, dy, dz)
  , [ (x + dx, y + dy, z + dz)
    | (x, y, z) <- ps
    ]
  )
  where
    dx = x0 - x1
    dy = y0 - y1
    dz = z0 - z1

rebasesTo :: Point -> [Point] -> [((Int, Int, Int), [Point])]
rebasesTo p = map (rebaseTo p) . shifts
  where
    shifts l = init $ zipWith (<>) (tails l) (inits l)

sync :: Scanner -> Scanner -> Maybe (Point, Scanner)
sync ps1 = safeHead . concatMap (syncs ps1) . evolutions
  where
    safeHead []    = Nothing
    safeHead (x:_) = Just x

syncs :: Scanner -> Scanner -> [(Point, Scanner)]
syncs ps1 ps2 =
  [ (s, ps)
  | p1 <- ps1
  , (s, ps) <- rebasesTo p1 ps2
  , let is = intersection ps1 ps
  , length is >= 12
  ]

syncAll :: Input -> Maybe [(Point, [Point])]
syncAll (s, ss) = go [((0, 0, 0), s)] ss
  where
    go cs [] = pure cs
    go cs xs = do
      (c, xs') <- extract (`trySync` cs) xs
      go (c:cs) xs'
    trySync _ [] = Nothing
    trySync x ((_, c):cs) = sync c x <|> trySync x cs

intersection :: (Foldable t, Eq a) => t a -> [a] -> [a]
intersection xs ys = [ y | y <- ys, y `elem` xs ]

evolutions :: Scanner -> [Scanner]
evolutions ps =
  [ map (o . r) ps
  | o <- orient
  , r <- rotate
  ]
  where
    rotate =
      [ id
      , \(x, y, z) -> (x, z, negate y)        -- CW 90
      , \(x, y, z) -> (x, negate y, negate z) -- CW 180
      , \(x, y, z) -> (x, negate z, y)        -- CW 270
      ]
    orient =
      [ id
      , \(x, y, z) -> (negate y, x, z)        -- L
      , \(x, y, z) -> (y, negate x, z)        -- R
      , \(x, y, z) -> (negate x, negate y, z) -- B
      , \(x, y, z) -> (z, y, negate x)        -- D
      , \(x, y, z) -> (negate z, y, x)        -- U
      ]

extract :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
extract f = go []
  where
    go _   []     = Nothing
    go acc (x:xs) = case f x of
      Just y  -> Just (y, acc <> xs)
      Nothing -> go (x:acc) xs

solution1, solution2 :: [(Point, [Point])] -> Int

solution1 = length . nub . concatMap snd

solution2 ps = maximum
  [ manhattan s1 s2
  | s1 <- ss
  , s2 <- ss
  , s1 /= s2
  ]
  where
    ss = map fst ps
    manhattan (x1, y1, z1) (x2, y2, z2) =
      abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

selfCheck :: IO ()
selfCheck = do
  e <- decode =<< readFile "Day19.example"
  Just ps <- pure $ syncAll e
  unless (solution1 ps == 79) $ fail "solution1"
  unless (solution2 ps == 3621) $ fail "solution2"
