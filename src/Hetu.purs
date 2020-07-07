module Hetu (parseHetu, HetuCentury, prettyPrintHetu, Hetu, intToBottom) where

import Prelude

import Data.Date (exactDate, year)
import Data.DateTime (Date, DateTime(..), Day, Month, Time(..), Year)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Int (fromString)
import Data.List (List(..), foldr, (:))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, singleton)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Text.Parsing.Parser (Parser, fail, parseErrorMessage, parseErrorPosition, runParser)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (anyChar, eof)
import Text.Parsing.Parser.Token (digit)

data HetuCentury
  = Minus
  | Plus
  | ALetter

instance showCentury :: Show HetuCentury where
  show Minus = "-"
  show Plus = "+"
  show ALetter = "A"

type Hetu = { date :: Date, id :: String }

yearFromHetu :: Int -> HetuCentury -> Maybe Year
yearFromHetu hetu letter = case letter of
  Minus -> toEnum $ 1900 + hetu
  Plus -> toEnum $ 1800 + hetu
  ALetter -> toEnum $ 2000 + hetu

centuryOf :: Hetu -> HetuCentury
centuryOf hetu = intYear hetu.date
  where
  intYear = yearToCentury <<< fromEnum <<< year
  yearToCentury y
    | y < 1900 = Plus
    | y >= 2000 = ALetter
    | otherwise = Minus

realCheckSum :: Date -> String -> Char
realCheckSum date id = remToCheckcsum (mod combined 31)
  where
  combined = unsafePartial (fromJust (fromString combinedMaybe))
  combinedMaybe = dateToSixLetter date <> id

dateToSixLetter :: Date -> String
dateToSixLetter d = format hetuFormat datetime
  where
  hetuFormat = DayOfMonthTwoDigits : MonthTwoDigits : YearTwoDigits : Nil
  datetime = dateTimeFromHetu d

prettyPrintHetu :: Hetu -> String
prettyPrintHetu hetu = dateToSixLetter (hetu.date) <> show century <> hetu.id <> singleton checkSumLetter
  where
  datetime = dateTimeFromHetu hetu.date
  century = centuryOf hetu
  hetuFormat = DayOfMonthTwoDigits : MonthTwoDigits : YearTwoDigits : Nil
  checkSumLetter = realCheckSum (hetu.date) (hetu.id)

intToBottom :: forall t. BoundedEnum t => Int -> t
intToBottom = fromMaybe bottom <<< toEnum

bottomEnum :: forall a. BoundedEnum a => a
bottomEnum = fromMaybe bottom $ toEnum 0

dateTimeFromHetu :: Date -> DateTime
dateTimeFromHetu d = DateTime d midnight
  where
  midnight = Time bottomEnum bottomEnum bottomEnum bottomEnum

concatInts :: List Int -> String
concatInts ints = foldr append "" $ ints <#> show

-- https://fi.wikipedia.org/wiki/Henkil%C3%B6tunnus#Tunnuksen_muoto
remToCheckcsum :: Int -> Char
remToCheckcsum 0 = '0'
remToCheckcsum 1 = '1'
remToCheckcsum 2 = '2'
remToCheckcsum 3 = '3'
remToCheckcsum 4 = '4'
remToCheckcsum 5 = '5'
remToCheckcsum 6 = '6'
remToCheckcsum 7 = '7'
remToCheckcsum 8 = '8'
remToCheckcsum 9 = '9'
remToCheckcsum 10 = 'A'
remToCheckcsum 11 = 'B'
remToCheckcsum 12 = 'C'
remToCheckcsum 13 = 'D'
remToCheckcsum 14 = 'E'
remToCheckcsum 15 = 'F'
remToCheckcsum 16 = 'H'
remToCheckcsum 17 = 'J'
remToCheckcsum 18 = 'K'
remToCheckcsum 19 = 'L'
remToCheckcsum 20 = 'M'
remToCheckcsum 21 = 'N'
remToCheckcsum 22 = 'P'
remToCheckcsum 23 = 'R'
remToCheckcsum 24 = 'S'
remToCheckcsum 25 = 'T'
remToCheckcsum 26 = 'U'
remToCheckcsum 27 = 'V'
remToCheckcsum 28 = 'W'
remToCheckcsum 29 = 'X'
remToCheckcsum 30 = 'Y'
remToCheckcsum remainder = unsafeCrashWith $ "illegal checksum: " <> (show remainder)

padToTwoLen :: Int -> String
padToTwoLen int = if length stringified < 2 then "0" <> stringified else stringified
  where
  stringified = show int

twoDigitNum :: Parser String Int
twoDigitNum = do
  left <- digit
  right <- digit
  case fromString $ fromCharArray [ left, right ] of
    Nothing -> fail "cannot parse two part number"
    Just n -> pure n

threeDigitNum :: Parser String Int
threeDigitNum = do
  left <- digit
  right <- digit
  righter <- digit
  case fromString $ fromCharArray [ left, right, righter ] of
    Nothing -> fail "cannot parse three part number"
    Just n -> pure n

parseMonth :: Parser String Month
parseMonth = do
  number <- twoDigitNum
  case toEnum number of
    Nothing -> fail "cannot parse month"
    Just month -> pure month

parseCentury :: Parser String HetuCentury
parseCentury = do
  c <- anyChar
  case c of
    '-' -> pure Minus
    '+' -> pure Plus
    'A' -> pure ALetter
    o -> fail $ "Invalid century: \"" <> singleton o <> "\""

parseId :: Parser String String
parseId = do
  first <- anyChar
  second <- anyChar
  third <- anyChar

  pure $ fromCharArray [ first, second, third ]

parseDay :: Parser String Day
parseDay = do
  rawDay <- twoDigitNum
  case toEnum rawDay of
    Nothing -> fail "Illegal day"
    Just d -> pure d

parseDate :: Parser String Date
parseDate = do
  day <- parseDay
  rawMonth <- twoDigitNum
  rawYear <- twoDigitNum
  century <- parseCentury

  case toEnum rawMonth of
    Nothing -> fail "Illegal month"
    Just month -> case yearFromHetu rawYear century of
      Nothing -> fail "Illegal year"
      Just year -> case exactDate year month day of
        Nothing -> fail "Illegal date combination"
        Just date -> pure date

hetuParser :: Parser String Hetu
hetuParser = do
  date <- parseDate
  id <- parseId
  rawCheckSum <- anyChar
  let calculatedCheckSum = realCheckSum date id
  eof

  if calculatedCheckSum == rawCheckSum
  then
    pure { date, id }
  else
    fail "Invalid checksum"

parseHetu :: String -> Either String Hetu
parseHetu input = case runParser input hetuParser of
  Left error -> Left $ parseErrorMessage error <> " at column " <> col
    where
    Position pos = parseErrorPosition error
    col = show pos.column
  Right success -> Right success
