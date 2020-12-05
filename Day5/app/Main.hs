module Main where

import           Data.Bits
import           Data.List

toSeat :: String -> Int
toSeat s = row * 8 + col
 where
  (rowChars, colChars) = splitAt 7 s
  col = foldl (\a c -> shift a 1 .|. (if c == 'L' then zeroBits else bit 0))
              0
              colChars
  row = foldl (\a c -> shift a 1 .|. (if c == 'F' then zeroBits else bit 0))
              0
              rowChars

missingSeat :: [Int] -> Int
missingSeat []           = error "failure"
missingSeat [_         ] = error "failure"
missingSeat (x : y : xs) = if x + 2 == y then x + 1 else missingSeat (y : xs)

main :: IO ()
main = do
  seats <- map toSeat . lines <$> readFile "input.txt"
  print $ maximum seats
  print $ missingSeat (sort seats)

