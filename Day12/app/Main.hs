{-# LANGUAGE DerivingStrategies #-}
module Main where

import           Data.Functor
import           Text.Parsec
import           Text.Parsec.String

data Boat = Boat Cardinal Int Int
  deriving stock Show

data Waypoint = Waypoint Int Int
  deriving stock Show

data Cardinal = N | E | S | W
  deriving stock (Show, Eq, Enum, Ord, Bounded)

data Rotate = L | R
  deriving stock Show

data Move = F
  deriving stock Show

data Direction = Cardinal Cardinal | Rotate Rotate | Move Move
  deriving stock Show

data Instr = Instr Direction Int
  deriving stock Show

direction :: Parser Direction
direction =
  (char 'F' $> Move F)
    <|> (char 'L' $> Rotate L)
    <|> (char 'R' $> Rotate R)
    <|> (char 'N' $> Cardinal N)
    <|> (char 'S' $> Cardinal S)
    <|> (char 'E' $> Cardinal E)
    <|> (char 'W' $> Cardinal W)

instruction :: Parser Instr
instruction = do
  d <- direction
  n <- read <$> many1 digit
  pure $ Instr d n

instructions :: Parser [Instr]
instructions = manyTill (instruction <* newline) (try eof)

solve2 :: Boat -> Waypoint -> [Instr] -> Int
solve2 (     Boat _ x y) _                         []       = abs x + abs y
solve2 boat@(Boat f x y) waypoint@(Waypoint wx wy) (i : is) = case i of
  Instr (Move F) n -> solve2 (Boat f (x + (wx * n)) (y + (wy * n))) waypoint is
  Instr (Rotate d) n -> case mod (div n 90) 4 of
    0 -> solve2 boat waypoint is
    1 -> case d of
      R -> solve2 boat (Waypoint wy (negate wx)) is
      L -> solve2 boat (Waypoint (negate wy) wx) is
    2 -> solve2 boat (Waypoint (negate wx) (negate wy)) is
    3 -> case d of
      R -> solve2 boat (Waypoint (negate wy) wx) is
      L -> solve2 boat (Waypoint wy (negate wx)) is
    _ -> error "do you even modulus"
  Instr (Cardinal c) n -> case c of
    N -> solve2 boat (Waypoint wx (wy + n)) is
    S -> solve2 boat (Waypoint wx (wy - n)) is
    E -> solve2 boat (Waypoint (wx + n) wy) is
    W -> solve2 boat (Waypoint (wx - n) wy) is

solve :: Boat -> [Instr] -> Int
solve (Boat _ x y) []       = abs x + abs y
solve (Boat f x y) (i : is) = case i of
  Instr (Move F) n -> case f of
    N -> solve (Boat f x (y + n)) is
    S -> solve (Boat f x (y - n)) is
    E -> solve (Boat f (x + n) y) is
    W -> solve (Boat f (x - n) y) is
  Instr (Rotate R) n ->
    let rotate = div n 90
        newDir = dropWhile (/= f) (cycle [minBound .. maxBound])
          !! max 0 (fromIntegral rotate)
    in  solve (Boat newDir x y) is
  Instr (Rotate L) n ->
    let rotate = div n 90
        newDir =
            dropWhile (/= f) (cycle $ reverse [minBound .. maxBound])
              !! max 0 (fromIntegral rotate)
    in  solve (Boat newDir x y) is
  Instr (Cardinal c) n -> case c of
    N -> solve (Boat f x (y + n)) is
    S -> solve (Boat f x (y - n)) is
    E -> solve (Boat f (x + n) y) is
    W -> solve (Boat f (x - n) y) is

main :: IO ()
main = do
  results <- parseFromFile instructions "input.txt"
  case results of
    Left  err    -> error $ show err
    Right instrs -> do
      print $ solve (Boat E 0 0) instrs
      print $ solve2 (Boat E 0 0) (Waypoint 10 1) instrs
