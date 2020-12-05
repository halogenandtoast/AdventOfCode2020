{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Main where

import           Data.Either        (isRight)
import           Data.Functor       (void, ($>))
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe         (fromMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Text.Parsec        hiding (between)
import           Text.Parsec.String (parseFromFile)
import           Text.Read          hiding (choice)

data Measurement = Cm | In

data PassportField = Byr | Iyr | Eyr | Hgt | Hcl | Ecl | Pid | Cid
  deriving stock (Show, Eq, Ord, Enum)

requiredFields :: Set PassportField
requiredFields = Set.fromList [Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid]

type Parser = Parsec String ()

newtype Passport = Passport { unPassport :: Map PassportField String }
  deriving newtype (Show)

height :: Parser (Int, Measurement)
height = do
  amount      <- maybe (fail "failed") pure . readMaybe =<< many1 digit
  measurement <- choice [string "cm" $> Cm, string "in" $> In]
  pure (amount, measurement)

passportField' :: Parser PassportField
passportField' = choice
  [ try (string "byr") $> Byr
  , try (string "iyr") $> Iyr
  , try (string "eyr") $> Eyr
  , try (string "hgt") $> Hgt
  , try (string "hcl") $> Hcl
  , try (string "ecl") $> Ecl
  , try (string "pid") $> Pid
  , try (string "cid") $> Cid
  ]

passportField :: Parser (PassportField, String)
passportField = do
  field <- passportField'
  _     <- char ':'
  value <- manyTill anyChar (try (oneOf " \n"))
  pure (field, value)

passports :: Parser [Passport]
passports = manyTill passport (try eof)

passport :: Parser Passport
passport = do
  fields <- manyTill passportField (void (try newline) <|> try eof)
  pure . Passport $ Map.fromList fields

hairColor :: Parser ()
hairColor = void $ char '#' >> count 6 hexDigit

eyeColor :: Parser ()
eyeColor = void $ choice
  [ try (string color)
  | color <- ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  ]

isValid :: Passport -> Bool
isValid = all (uncurry isValidField) . Map.toList . unPassport

between :: Int -> Int -> Int -> Bool
between x y a = a >= x && a <= y

willParse :: Parser a -> String -> Bool
willParse p str = isRight (parse p "" str)

isValidField :: PassportField -> String -> Bool
isValidField Byr val = maybe False (between 1920 2002) (readMaybe val)
isValidField Iyr val = maybe False (between 2010 2020) (readMaybe val)
isValidField Eyr val = maybe False (between 2020 2030) (readMaybe val)
isValidField Hgt val = case parse height "" val of
  Left  _                     -> False
  Right (amount, measurement) -> case measurement of
    Cm -> between 150 193 amount
    In -> between 59 76 amount
isValidField Hcl val = willParse hairColor val
isValidField Ecl val = willParse eyeColor val
isValidField Pid val = willParse (count 9 digit) val && length val == 9
isValidField Cid val = True

main :: IO ()
main = do
  result <- parseFromFile passports "input.txt"
  case result of
    Left  failure -> error $ show failure
    Right success -> do
      let present = filter
            (Set.isSubsetOf requiredFields . Map.keysSet . unPassport)
            success
      print $ length present
      let valid = filter isValid present
      print $ length valid
