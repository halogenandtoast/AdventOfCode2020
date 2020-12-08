{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
module Main where

import           Text.Parsec
import           Text.Parsec.String

data Instr = Nop Int | Acc Int | Jmp Int
  deriving stock Show

data Program = Program
  { ppc       :: Int
  , pacc      :: Int
  , pvisited  :: [Int]
  , pmodified :: Bool
  , pinstrs   :: [Instr]
  }
  deriving stock Show

instr :: Parser Instr
instr = (nop <|> acc <|> jmp) <*> (spaces >> signedNumber)
 where
  nop = string "nop" >> pure Nop
  acc = string "acc" >> pure Acc
  jmp = string "jmp" >> pure Jmp

signedNumber :: Parser Int
signedNumber = (plus <|> minus) <*> number
 where
  plus   = char '+' >> pure id
  minus  = char '-' >> pure negate
  number = read <$> many1 digit

program :: Parser Program
program = Program 0 0 [] False <$> manyTill (instr <* try newline) eof

run2 :: Bool -> Program -> Either Program Program
run2 canFix pgm@Program {..}
  | ppc `elem` pvisited = Left pgm
  | ppc >= length pinstrs = Right pgm
  | otherwise = case pinstrs !! ppc of
    Nop n -> if pmodified || not canFix
      then run2 canFix nopV
      else either (\_ -> run2 canFix ((jmpV n) { pmodified = True }))
                  Right
                  (run2 canFix nopV)
    Jmp n -> if pmodified || not canFix
      then run2 canFix (jmpV n)
      else either (\_ -> run2 canFix (nopV { pmodified = True }))
                  Right
                  (run2 canFix (jmpV n))
    Acc n -> run2 canFix
      $ pgm { pacc = pacc + n, ppc = ppc + 1, pvisited = ppc : pvisited }
 where
  nopV = pgm { ppc = ppc + 1, pvisited = ppc : pvisited }
  jmpV n = pgm { ppc = ppc + n, pvisited = ppc : pvisited }

main :: IO ()
main = do
  result <- parseFromFile program "input.txt"
  case result of
    Left  err -> error $ show err
    Right pgm -> do
      print $ either pacc pacc (run2 False pgm)
      print $ either (error "failed") pacc (run2 True pgm)
