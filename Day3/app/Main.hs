{-# LANGUAGE DerivingStrategies #-}
module Main where

import           Data.Foldable
import           Text.Parsec

type Parser = Parsec String ()

data Cell = Tree | Ground
  deriving stock (Eq, Show)

tree :: Parser Cell
tree = char '#' >> pure Tree

ground :: Parser Cell
ground = char '.' >> pure Ground

cell :: Parser Cell
cell = tree <|> ground

cellMap :: Parser [[Cell]]
cellMap = cellMapLine `sepBy` newline

cellMapLine :: Parser [Cell]
cellMapLine = many cell

readMap :: String -> IO [[Cell]]
readMap filename = do
  contents <- readFile filename
  case parse cellMap filename contents of
    Left  _      -> error "failed to parse"
    Right result -> pure $ filter (not . null) result

solve :: [[Cell]] -> Int -> Int -> Int
solve tMap slopeX slopeY = go slopeX slopeY 0
 where
  go _ y n | y >= mapHeight = n
  go x y n                  = do
    go ((x + slopeX) `mod` mapWidth) (y + slopeY)
      $ if (tMap !! y) !! x == Tree then n + 1 else n
  mapHeight = length tMap
  mapWidth  = length (head tMap)

main :: IO ()
main = do
  topologicalMap <- readMap "input.txt"
  print $ solve topologicalMap 3 1
  let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

  traverse_ (print . uncurry (solve topologicalMap)) slopes
  print $ product $ map (uncurry (solve topologicalMap)) slopes
