module Main where

import           Data.List
import           Data.Maybe
import           Text.Read

solve :: [Int] -> Maybe Int
solve []       = Nothing
solve (x : xs) = case find ((== 2020) . (+ x)) xs of
  Nothing -> solve xs
  Just y  -> Just (x * y)

main :: IO ()
main = do
  content <- mapMaybe readMaybe . lines <$> readFile "input.txt"
  case solve content of
    Nothing       -> error "invalid input"
    Just solution -> print solution

