{-# LANGUAGE DerivingStrategies #-}
module Main where

import           Data.Functor
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Text.Parsec
import           Text.Parsec.String

type Color = String
type Rule = Map Color [(Int, Color)]

canContain :: String -> Rule -> [(Int, Color)] -> Bool
canContain _ _ [] = False
canContain color _ currentColors | color `elem` map snd currentColors = True
canContain color ruleMap (x : xs) =
  canContain color ruleMap (xs <> Map.findWithDefault [] (snd x) ruleMap)

solve :: Rule -> Int
solve ruleMap =
  length $ filter (canContain "shiny gold" ruleMap) (Map.elems ruleMap)

solve2 :: Rule -> Int
solve2 ruleMap = go start
 where
  get c = Map.findWithDefault [] c ruleMap
  start = get "shiny gold"
  go []            = 0
  go ((n, c) : xs) = (n + n * go (get c)) + go xs

contain :: Parser (Int, Color)
contain = do
  n     <- read <$> (many1 digit <* spaces)
  color <- unwords <$> count 2 (many1 letter <* spaces)
  _     <- string "bag" >> optional (char 's')
  pure (n, color)

rule :: Parser Rule
rule = do
  color <- unwords <$> count 2 (many1 letter <* spaces)
  _     <-
    string "bag" >> optional (char 's') >> spaces >> string "contain" >> spaces
  allowed <-
    try (string "no other bags" $> []) <|> (contain `sepBy1` string ", ")
  _ <- char '.'
  pure $ Map.singleton color allowed

main :: IO ()
main = do
  contents <- lines <$> readFile "input.txt"
  let rules = Map.unions
        $ map (either (error "invalid rule") id . parse rule "") contents
  print $ solve rules
  print $ solve2 rules
