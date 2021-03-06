{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}
module Main where

import           Data.List
import           Data.Map  (Map)
import qualified Data.Map  as Map

solve :: [Int] -> Int
solve = go 0 0 . (0 :)
 where
  go a b []             = a * b
  go a b [_]            = a * b
  go a b (x1 : x2 : xs) | x2 - x1 == 1 = go (a + 1) b (x2 : xs)
  go a b (x1 : x2 : xs) | x2 - x1 == 2 = go a b (x2 : xs)
  go a b (x1 : x2 : xs) | x2 - x1 == 3 = go a (b + 1) (x2 : xs)
  go _ _ _              = error "messed up"

solve2 :: Map Int Int -> [Int] -> Int
solve2 _ []       = 0
solve2 m [x     ] = sum [ Map.findWithDefault 0 (x - n) m | n <- [1 .. 3] ]
solve2 m (x : xs) = let y = solve2 m [x] in solve2 (Map.insert x y m) xs

main :: IO ()
main = do
  adapters <- map (read @Int) . lines <$> readFile "input.txt"
  let builtInAdapter = maximum adapters + 3
      allAdapters    = sort adapters <> [builtInAdapter]
  print $ solve allAdapters
  print $ solve2 (Map.singleton 0 1) allAdapters
