{-# OPTIONS -Wall #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Main where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except

type Bits = [Bool]
type Packet = (Int, Body)
data Body
  = Literal Int
  | Op Int [Packet]
  deriving Show
type Input = Packet

newtype DecoderM a = DecoderM (StateT Bits (Except String) a)
  deriving (Functor, Applicative, Monad, MonadState Bits, MonadError String)

runDecoderM :: DecoderM a -> Bits -> Either String (a, Bits)
runDecoderM (DecoderM action) = runExcept . runStateT action

main :: IO ()
main = do
  selfCheck
  Just i <- decode . init <$> readFile "Day16.input"
  print $ solution1 i
  print $ solution2 i

solution1, solution2 :: Input -> Int

solution1 = addVersions
  where
    addVersions (v, b) = v + case b of
      Op _ ps -> sum $ map addVersions ps
      _       -> 0

solution2 = eval
  where
    eval (_, b) = case b of
      Literal x -> x
      Op o ps   ->
        (case o of
            0 -> sum
            1 -> product
            2 -> minimum
            3 -> maximum
            5 -> binOp (>)
            6 -> binOp (<)
            7 -> binOp (==)
            _ -> error $ "Unknown op: " <> show o
        ) $ map eval ps
    binOp f [x, y] = if f x y then 1 else 0
    binOp _ args   = error $ "Wrong number of args: " <> show args

decode :: String -> Maybe Input
decode s = do
  bits <- concat <$> traverse fromHex s
  either error (Just . fst) $ runDecoderM packetD bits

packetD :: DecoderM Packet
packetD = do
  version <- fromBits <$> chop 3
  body <- fromBits <$> chop 3 >>= \case
    4 -> literalD
    o -> opD o
  pure (version, body)

literalD :: DecoderM Body
literalD = Literal . fromBits . concat <$> go
  where
    go = do
      flag <- chopBit
      bs <- chop 4
      if flag
        then (bs :) <$> go
        else pure [bs]

opD :: Int -> DecoderM Body
opD code = do
  flag <- chopBit
  ps <-
    if flag
    then do
      n <- fromBits <$> chop 11
      replicateM n packetD
    else do
      s <- fromBits <$> chop 15
      bs <- chop s
      case packets bs of
        Left err -> throwError err
        Right xs -> pure xs
  pure $ Op code ps

packets :: Bits -> Either String [Packet]
packets [] = pure []
packets bs = do
  (p, bs') <- runDecoderM packetD bs
  (p :) <$> packets bs'

chopBit :: DecoderM Bool
chopBit = chop 1 >>= \case
  [x] -> pure x
  _   -> error "Impossible"

chop :: Int -> DecoderM Bits
chop n = do
  peek n >>= \case
    Just bs -> bs <$ skip n
    Nothing -> throwError "Not enough bits"

peek :: Int -> DecoderM (Maybe Bits)
peek n = do
  bs <- take n <$> get
  pure $ if length bs == n
    then Just bs
    else Nothing

skip :: Int -> DecoderM ()
skip n = modify $ drop n

fromBits :: Bits -> Int
fromBits =
  sum . zipWith select powers . reverse
  where
    select x c
      | c         = x
      | otherwise = 0
    powers = iterate (* 2) 1

fromHex :: Char -> Maybe [Bool]
fromHex = \case
  '0' -> Just [False, False, False, False]
  '1' -> Just [False, False, False, True ]
  '2' -> Just [False, False, True,  False]
  '3' -> Just [False, False, True,  True ]
  '4' -> Just [False, True,  False, False]
  '5' -> Just [False, True,  False, True ]
  '6' -> Just [False, True,  True,  False]
  '7' -> Just [False, True,  True,  True ]
  '8' -> Just [True,  False, False, False]
  '9' -> Just [True,  False, False, True ]
  'A' -> Just [True,  False, True,  False]
  'B' -> Just [True,  False, True,  True ]
  'C' -> Just [True,  True,  False, False]
  'D' -> Just [True,  True,  False, True ]
  'E' -> Just [True,  True,  True,  False]
  'F' -> Just [True,  True,  True,  True ]
  _   -> Nothing

selfCheck :: MonadFail m => m ()
selfCheck = do
  Just i1 <- pure $ decode "A0016C880162017C3686B18A3D4780"
  unless (solution1 i1 == 31) $ fail "solution1"
  Just i2 <- pure $ decode "9C0141080250320F1802104A08"
  unless (solution2 i2 == 1) $ fail "solution2"
