{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DerivingStrategies #-}

module Main where

import           Data.Bits
import           Data.Functor
import           Data.List
import qualified Data.Map.Strict    as Map
import           Text.Parsec
import           Text.Parsec.String

data Instr = SetMask Mask | SetMem Int Int
  deriving stock Show

type Program = [Instr]

type Mask = [MaskBit]
data MaskBit = One | Zero | X
  deriving stock (Eq, Show)

maskBit :: Parser MaskBit
maskBit = (char 'X' $> X) <|> (char '1' $> One) <|> (char '0' $> Zero)

instr :: Parser Instr
instr = (try (string "mask = ") *> (SetMask <$> many1 maskBit)) <|> memory

memory :: Parser Instr
memory = do
  _ <- string "mem["
  n <- read <$> many1 digit
  _ <- string "] = "
  x <- read <$> many1 digit
  pure $ SetMem n x

program :: Parser Program
program = manyTill (instr <* try newline) (try eof)

solve :: [Instr] -> Int
solve = go (replicate 36 X) mempty
 where
  go _ ram []                   = sum ram
  go _ ram (SetMask mask' : is) = go mask' ram is
  go mask ram (SetMem addr val : is) =
    go mask (Map.alter (Just . const (applyMaskValue mask val)) addr ram) is

solve2 :: [Instr] -> Int
solve2 = go (replicate 36 Zero) mempty
 where
  go _ ram []                   = sum ram
  go _ ram (SetMask mask' : is) = go mask' ram is
  go mask ram (SetMem addr val : is) =
    let addrs = maskToAddrs (updateMask mask addr)
    in  go mask (foldl' (\ram' b -> Map.insert b val ram') ram addrs) is

updateMask :: [MaskBit] -> Int -> [MaskBit]
updateMask mbs i = go (zip mbs [35, 34 .. 0])
 where
  go []               = []
  go ((mb, n) : rest) = case mb of
    X    -> X : go rest
    Zero -> (if testBit i n then One else Zero) : go rest
    One  -> One : go rest

maskToAddrs :: [MaskBit] -> [Int]
maskToAddrs = go [0]
 where
  go !xs []       = xs
  go !xs (b : bs) = case b of
    Zero -> go (map (`shift` 1) xs) bs
    One  -> go (map (\x -> setBit (shift x 1) 0) xs) bs
    X    -> go (concatMap (\x -> [shift x 1, setBit (shift x 1) 0]) xs) bs

applyMaskValue :: [MaskBit] -> Int -> Int
applyMaskValue m val = foldl' applyBit val (zip m [35, 34 .. 0])

applyBit :: Int -> (MaskBit, Int) -> Int
applyBit val (b, a) = case b of
  X    -> val
  Zero -> clearBit val a
  One  -> setBit val a

main :: IO ()
main = do
  eresult <- parseFromFile program "input.txt"
  case eresult of
    Left  err    -> error $ show err
    Right result -> do
      print $ solve result
      print $ solve2 result
