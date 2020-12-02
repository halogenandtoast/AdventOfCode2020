{-# LANGUAGE RecordWildCards #-}
module Main where

import           Text.Parsec hiding (count)

count :: (a -> Bool) -> [a] -> Int
count = (length .) . filter

data Entry = Entry
  { entryMin      :: Int
  , entryMax      :: Int
  , entryChar     :: Char
  , entryPassword :: String
  }

isValid :: Entry -> Bool
isValid Entry {..} = occurrences >= entryMin && occurrences <= entryMax
  where occurrences = count (== entryChar) entryPassword

isValid2 :: Entry -> Bool
isValid2 Entry {..} =
  (firstChar == entryChar || secondChar == entryChar)
    && (firstChar /= secondChar)
 where
  firstChar  = entryPassword !! (entryMin - 1)
  secondChar = entryPassword !! (entryMax - 1)

entryP :: Parsec String () Entry
entryP = do
  entryMin      <- read <$> many1 digit
  _             <- char '-'
  entryMax      <- read <$> many1 digit
  _             <- spaces
  entryChar     <- letter
  _             <- char ':'
  _             <- spaces
  entryPassword <- many1 letter
  pure $ Entry { .. }

main :: IO ()
main = do
  contents <- lines <$> readFile "input.txt"
  print $ count
    (\line -> case parse entryP "" line of
      Left  _     -> error "invalid line entry"
      Right entry -> isValid entry
    )
    contents

  print $ count
    (\line -> case parse entryP "" line of
      Left  _     -> error "invalid line entry"
      Right entry -> isValid2 entry
    )
    contents
