module Main where

import           Data.List
import           Data.Maybe
import           Text.Read

solve :: [Int] -> Maybe Int
solve []       = Nothing
solve (x : xs) = case solve2 x xs of
  Just z  -> Just z
  Nothing -> solve xs

solve2 :: Int -> [Int] -> Maybe Int
solve2 _ []       = Nothing
solve2 x (y : ys) = case find ((== 2020) . (+ (y + x))) ys of
  Nothing -> solve2 x ys
  Just z  -> Just (x * y * z)

main :: IO ()
main = do
  content <- mapMaybe readMaybe . lines <$> readFile "input.txt"
  case solve content of
    Nothing       -> error "invalid input"
    Just solution -> print solution
