{-# LANGUAGE DerivingStrategies #-}
module Main where

import           Data.Functor
import           Data.Maybe
import           Data.Vector        (Vector, (!?))
import qualified Data.Vector        as V
import           Text.Parsec
import           Text.Parsec.String

type Grid = Vector (Vector Location)

data Location = Floor | EmptySeat | OccupiedSeat
  deriving stock Eq

solve :: (Grid -> Grid) -> Grid -> Int
solve f layout = sum . V.map (V.length . V.filter (== OccupiedSeat)) $ findFix
  layout
  where findFix l = let l' = f l in if l == l' then l else findFix l'

ferry :: Parser Grid
ferry = V.fromList <$> manyTill (ferryRow <* newline) (try eof)

ferryRow :: Parser (Vector Location)
ferryRow = V.fromList <$> many1 location

location :: Parser Location
location =
  (char '.' $> Floor) <|> (char 'L' $> EmptySeat) <|> (char '#' $> OccupiedSeat)

nextGen :: Grid -> Grid
nextGen base = V.imap rowMod base
 where
  rowMod i = V.imap (locationMod i)
  locationMod i j s = case s of
    EmptySeat    -> if neighborCount i j == 0 then OccupiedSeat else EmptySeat
    OccupiedSeat -> if neighborCount i j >= 4 then EmptySeat else OccupiedSeat
    _            -> s
  neighborCount i j = length . filter (== OccupiedSeat) $ catMaybes
    [ (base !? i') >>= (!? j')
    | j' <- [(j - 1) .. (j + 1)]
    , i' <- [(i - 1) .. (i + 1)]
    , i' /= i || j' /= j
    ]

nextGen2 :: Grid -> Grid
nextGen2 base = V.imap rowMod base
 where
  rowMod i = V.imap (locationMod i)
  locationMod i j s = case s of
    EmptySeat ->
      if visibleNeighborCount i j == 0 then OccupiedSeat else EmptySeat
    OccupiedSeat ->
      if visibleNeighborCount i j >= 5 then EmptySeat else OccupiedSeat
    _ -> s
  directions =
    [(1, 0), (0, 1), (-1, 0), (0, -1), (1, -1), (-1, 1), (-1, -1), (1, 1)]
  findSeat y x (dy, dx) = case base !? (y + dy) >>= (!? (x + dx)) of
    Nothing           -> Floor
    Just EmptySeat    -> EmptySeat
    Just OccupiedSeat -> OccupiedSeat
    Just Floor        -> findSeat (y + dy) (x + dx) (dy, dx)
  visibleNeighborCount y x =
    length . filter (== OccupiedSeat) $ map (findSeat y x) directions

main :: IO ()
main = do
  results <- parseFromFile ferry "input.txt"
  case results of
    Left  err       -> error $ show err
    Right ferryGrid -> do
      print $ solve nextGen ferryGrid
      print $ solve nextGen2 ferryGrid
