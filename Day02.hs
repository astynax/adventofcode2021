{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Text.Read (readMaybe)

data Cmd = F | D | U

main = do
  selfCheck
  xs <- mapM decodeLine . lines =<< readFile "Day02.input"
  print $ solution1 xs
  print $ solution2 xs

decodeLine :: MonadFail m => String -> m (Cmd, Int)
decodeLine l = case words l of
  [cmd, arg] -> case readMaybe arg of
    Just v -> case cmd of
      "forward" -> pure (F, v)
      "down"    -> pure (D, v)
      "up"      -> pure (U, v)
      _         -> reject "Bad command"
    _ -> reject "Bad argument"
  _ -> reject "Bad input"
  where
    reject reason = fail $ reason <> ": " <> l

solution1 cs =
  let (x, y) = mapM_ step cs `execState` (0, 0)
  in x * y
  where
    step = \case
      (F, v) -> modify $ \(x, y) -> (x + v, y)
      (D, v) -> modify $ \(x, y) -> (x, y + v)
      (U, v) -> modify $ \(x, y) -> (x, y - v)

solution2 cs =
  let (_, x, y) = mapM_ step cs `execState` (0, 0, 0)
  in x * y
  where
    step = \case
      (F, v) -> modify $ \(a, x, y) -> (a, x + v, y + v * a)
      (D, v) -> modify $ \(a, x, y) -> (a + v, x, y)
      (U, v) -> modify $ \(a, x, y) -> (a - v, x, y)

selfCheck = do
  example <- mapM decodeLine
    [ "forward 5"
    , "down 5"
    , "forward 8"
    , "up 3"
    , "down 8"
    , "forward 2"
    ]
  unless (solution1 example == 150) $ fail "Step 1"
  unless (solution2 example == 900) $ fail "Step 2"
