{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}
module Main where

import           Data.Bifunctor
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.String

data Notes = Notes Int [Maybe Int]
  deriving stock Show

busIds :: Parser [Maybe Int]
busIds =
  sepBy1 (Just . read <$> many1 digit <|> char 'x' $> Nothing) (char ',')

notes :: Parser Notes
notes = do
  earliest <- read <$> many1 digit
  _        <- newline
  Notes earliest <$> busIds

solve2 :: Notes -> Int
solve2 (Notes _ ns) = go 0 (fst $ head ns') ns'
 where
  ns' = map (first fromJust) . filter (isJust . fst) $ zip ns [0 ..]
  go _ _ []  = error "should not happen"
  go t _ [_] = t
  go t inc zs@((x, mx) : (y, my) : rest) =
    if mod (t + mx) x == 0 && mod (t + my) y == 0
      then go t (inc * y) ((y, my) : rest)
      else go (t + inc) inc zs

solve :: Notes -> Int
solve (Notes s bids) =
  case
      minimumBy
        (compare `on` snd)
        (map
          (\b -> (b, ceiling @Double (fromIntegral s / fromIntegral b) * b))
          (catMaybes bids)
        )
    of
      (bid, n) -> bid * (n - s)

main :: IO ()
main = do
  results <- parseFromFile notes "input.txt"
  case results of
    Left  err -> error $ show err
    Right n   -> do
      print $ solve n
      print $ solve2 n
