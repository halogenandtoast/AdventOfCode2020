{-# LANGUAGE DerivingStrategies #-}
module Main where

solve :: [[Char]] -> Int -> Int -> Int
solve tMap slopeX slopeY = go slopeX slopeY 0
 where
  go _ y n | y >= mapHeight = n
  go x y n                  = do
    go ((x + slopeX) `mod` mapWidth) (y + slopeY)
      $ if (tMap !! y) !! x == '#' then n + 1 else n
  mapHeight = length tMap
  mapWidth  = length (head tMap)

main :: IO ()
main = do
  topologicalMap <- lines <$> readFile "input.txt"
  print $ solve topologicalMap 3 1
  let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  print $ product $ map (uncurry (solve topologicalMap)) slopes
