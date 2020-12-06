module Main where

import           Prelude

import           Control.Monad
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Text.Parsec
import           Text.Parsec.String

response :: Parser (Set Char)
response = Set.fromList <$> manyTill letter (try newline)

responses :: Parser [Set Char]
responses = manyTill response (try eof <|> void (try newline))

customs :: Parser [[Set Char]]
customs = manyTill responses (try eof)

main :: IO ()
main = do
  result <- parseFromFile customs "input.txt"
  case result of
    Left  err   -> error $ show err
    Right forms -> do
      print $ sum $ map (Set.size . Set.unions) forms
      print $ sum $ map (Set.size . foldl1 Set.intersection) forms

