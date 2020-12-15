module Main where

import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Text.Parsec
import           Text.Parsec.String

solve :: Int -> Int -> Map Int Int -> Int
solve 2020 n _     = n
solve gen  n cache = case Map.lookup n cache of
  Just x  -> solve (gen + 1) (gen - x) (Map.insert n gen cache)
  Nothing -> solve (gen + 1) 0 (Map.insert n gen cache)

-- quite a bit slow, but still solved it
solve2 :: Int -> Int -> Map Int Int -> Int
solve2 30000000 n _     = n
solve2 gen      n cache = case Map.lookup n cache of
  Just x  -> solve2 (gen + 1) (gen - x) (Map.insert n gen cache)
  Nothing -> solve2 (gen + 1) 0 (Map.insert n gen cache)

doParse :: Parser (Int, Map Int Int)
doParse = do
  numbers <- fmap read <$> many1 digit `sepBy` comma
  pure (last numbers, Map.fromList $ zip (init numbers) [1 ..])
  where comma = char ','

main :: IO ()
main = do
  eresult <- parseFromFile doParse "input.txt"
  case eresult of
    Left  err        -> error $ show err
    Right (n, cache) -> do
      print $ solve (Map.size cache + 1) n cache
      print $ solve2 (Map.size cache + 1) n cache
