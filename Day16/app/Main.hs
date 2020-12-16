{-# LANGUAGE DerivingStrategies #-}

module Main where

import           Data.Foldable
import           Data.List
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Text.Parsec
import           Text.Parsec.String

data Range = Range Int Int
  deriving stock Show

data Field = Field
  { fieldLabel  :: String
  , fieldRanges :: [Range]
  }
  deriving stock Show

type Ticket = [Int]

data Problem = Problem [Field] Ticket [Ticket]
  deriving stock Show

range :: Parser Range
range =
  Range <$> (read <$> many1 digit) <*> (char '-' *> (read <$> many1 digit))

ticket :: Parser Ticket
ticket = (read <$> many1 digit) `sepBy1` char ','

field :: Parser Field
field = do
  l <- manyTill anyChar (char ':') <* char ' '
  Field l <$> range `sepBy1` string " or "

problem :: Parser Problem
problem = do
  fields     <- manyTill (field <* newline) newline
  _          <- string "your ticket:" >> newline
  yourTicket <- ticket <* newline
  _          <- newline >> string "nearby tickets:" >> newline
  tickets    <- manyTill (ticket <* try newline) eof
  pure $ Problem fields yourTicket tickets

solve :: Problem -> Int
solve (Problem fields _ tickets) = sum $ filter isInvalid values
 where
  ranges = concatMap fieldRanges fields
  values = concat tickets
  isInvalid val =
    not $ any (\(Range low high) -> val >= low && val <= high) ranges

solve2 :: Problem -> Int
solve2 (Problem fields yourTicket tickets) = product $ map
  (\f ->
    yourTicket !! Map.findWithDefault (error "missing") (fieldLabel f) fieldMap
  )
  departureFields
 where
  departureFields = filter (isPrefixOf "departure" . fieldLabel) fields
  validTickets    = filter isValidTicket tickets
  isValidTicket vals = all isValidValue vals
  isValidValue val = any (inFieldRanges val . fieldRanges) fields
  inFieldRanges val rs =
    any (\(Range low high) -> val >= low && val <= high) rs
  fieldMap = solveFieldMap $ foldl' findColumn Map.empty fields
  findColumn m f =
    let ns = filter (allInRanges (fieldRanges f)) [0 .. length fields - 1]
    in  Map.insert (fieldLabel f) ns m
  allInRanges rs n = all (\vs -> inFieldRanges (vs !! n) rs) validTickets

solveFieldMap :: Map String [Int] -> Map String Int
solveFieldMap m = if all onlyOne asList
  then Map.map head m
  else solveFieldMap $ Map.fromList (map filterSingletons asList)
 where
  asList     = Map.toList m
  onlyOne    = (== 1) . length . snd
  singletons = filter onlyOne asList
  filterSingletons (f, ns) = (f, foldl' (removeSingleton f) ns singletons)
  removeSingleton f ns (f', _)  | f == f' = ns
  removeSingleton _ ns (_, rns) = ns \\ rns

main :: IO ()
main = do
  eresult <- parseFromFile problem "input.txt"
  case eresult of
    Left  err    -> error $ show err
    Right result -> do
      print $ solve result
      print $ solve2 result
