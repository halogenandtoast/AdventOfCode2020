{-# LANGUAGE TypeApplications #-}
module Main where

solve :: Int -> [Int] -> Int
solve _ []           = error "no solution"
solve l xs@(_ : xs') = if valid then solve l xs' else y
 where
  (preamble, y : _) = splitAt l xs
  valid             = or [ a /= b && a + b == y | a <- preamble, b <- preamble ]

solve2 :: Int -> [Int] -> Int
solve2 _      []       = error "no solution"
solve2 target (x : xs) = go [x] xs
 where
  go cont ys
    | length cont > 1 && sum cont == target
    = minimum cont + maximum cont
    | sum cont > target
    = let (_  : pref) = cont
          (x' : xs' ) = pref <> ys
      in  go [x'] xs'
    | otherwise
    = let (y' : ys') = ys in go (cont <> [y']) ys'

main :: IO ()
main = do
  ints <- map (read @Int) . lines <$> readFile "input.txt"
  let target = solve 25 ints
  print target
  print $ solve2 target ints
