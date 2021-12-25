{-# OPTIONS -Wall #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import "massiv" Data.Massiv.Core
import "massiv" Data.Massiv.Array (U)
import "massiv" Data.Massiv.Array.Mutable as M
import Data.Foldable

type Field = Array U Ix2 Char
type MField = MArray (PrimState IO) U Ix2 Char

main :: IO ()
main = do
  selfCheck
  i <- decode =<< readFile "Day25.input"
  print =<< solution1 i
  print =<< solution2 i

solution1, solution2 :: Field -> IO Int

solution1 f = thaw f >>= go 1
  where
    go n arr = do
      x <- mstep arr
      if x == 0
        then pure n
        else go (n + 1) arr

solution2 = const $ pure 0

decode :: String -> IO Field
decode s = do
  ls@(l:_) <- pure $ lines s
  let w = length l
  let h = length ls
  arr <- initializeNew @U (Just '.') $ Sz2 w h
  for_ (zip [0..] ls) $ \(y, row) ->
    for_ (zip [0..] row) $ \(x, c) ->
      modify_ arr (const $ pure c) (x :. y)
  freeze Seq arr

step :: Field -> IO Field
step f = do
  mf <- thaw f
  _ <- mstep mf
  freeze Seq mf

mstep :: MField -> IO Int
mstep arr = do
  ls <- foldM (scan arr '>') []
    [ (px :. y, x :. y)
    | y <- ys
    , (px, x) <- zip ((w - 1) : init xs) xs]
  apply arr ls
  ds <- foldM (scan arr 'v') []
    [ (x :. py, x :. y)
    | x <- xs
    , (py, y) <- zip ((h - 1) : init ys) ys]
  apply arr ds
  pure $ length ls + length ds
  where
    Sz2 w h = sizeOfMArray arr
    ys = [0 .. h - 1]
    xs = [0 .. w - 1]
    scan a v l pair@(p1, p2) = do
      p <- M.readM a p1
      c <- M.readM a p2
      pure $
        if (c == '.' && p == v)
        then (v, pair) : l
        else l
    apply a = mapM_ $ \(c, (p1, p2)) -> do
      modify_ a (const $ pure '.') p1
      modify_ a (const $ pure c)   p2

selfCheck :: IO ()
selfCheck = do
  i <- decode =<< readFile "Day25.example"
  s1 <- solution1 i
  unless (s1 == 58) $ fail "solution1"

display :: Field -> IO ()
display f = do
  arr <- thaw f
  let Sz2 w h = sizeOfMArray arr
  for_ [0 .. h - 1] $ \y -> do
    for_ [0 .. w - 1] $ \x ->
      M.readM arr (x :. y) >>= putChar
    putChar '\n'
