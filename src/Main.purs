module Main (parseHetu, HetuDecade, prettyPrintHetu, Hetu) where

import Prelude

import Data.DateTime (Date, DateTime(..), Day, Month, Time(..), Year, canonicalDate)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, toEnum)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Int (fromString)
import Data.List (List(..), fold, foldr, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, singleton)
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser (Parser, fail, parseErrorMessage, parseErrorPosition, runParser)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (anyChar, eof)
import Text.Parsing.Parser.Token (digit)

data HetuDecade
  = Minus
  | Plus
  | ALetter

instance showDecade :: Show HetuDecade where
  show Minus = "-"
  show Plus = "+"
  show ALetter = "A"

type Hetu
  = { day :: Day
    , month :: Month
    , twoYearDigits :: Int
    , decade :: HetuDecade
    , id :: String
    , checkSum :: Char
    }

prettyPrintHetu :: Hetu -> String
prettyPrintHetu hetu = format hetuFormat datetime <> decade <> hetu.id <> checkSumLetter
  where
  datetime = dateTimeFromHetu hetu
  decade = show hetu.decade
  hetuFormat = DayOfMonthTwoDigits : MonthTwoDigits : YearTwoDigits : Nil
  checkSumLetter = singleton hetu.checkSum

yearFromHetu :: Hetu -> Year
yearFromHetu hetu = case hetu.decade of
  Minus -> intToBottom $ 1900 + hetu.twoYearDigits
  Plus -> intToBottom $ 1800 + hetu.twoYearDigits
  ALetter -> intToBottom $ 2000 + hetu.twoYearDigits

dateFromHetu :: Hetu -> Date
dateFromHetu hetu = canonicalDate (yearFromHetu hetu) hetu.month hetu.day

intToBottom :: forall t. BoundedEnum t => Int -> t
intToBottom = fromMaybe bottom <<< toEnum

bottomEnum :: forall a. BoundedEnum a => a
bottomEnum = fromMaybe bottom $ toEnum 0

dateTimeFromHetu :: Hetu -> DateTime
dateTimeFromHetu hetu = DateTime d midnight
  where
  midnight = Time bottomEnum bottomEnum bottomEnum bottomEnum
  d = dateFromHetu hetu

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

checkSum :: Int -> Int -> Int -> String -> Char
checkSum rawDay rawMonth rawYear id = remToCheckcsum $ mod concattedNum 31
  where
  concattedNum = case fromString $ concattedStr <> id of
    Nothing -> unsafeCrashWith $ "cannot parse id: " <> id
    Just a -> a
  concattedStr = fold $ padToTwoLen <$> rawDay : rawMonth : rawYear : Nil

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

parseDecade :: Parser String HetuDecade
parseDecade = do
  c <- anyChar
  case c of
    '-' -> pure Minus
    '+' -> pure Plus
    'A' -> pure ALetter
    o -> fail $ "invalid decade: " <> singleton o

parseId :: Parser String String
parseId = do
  first <- anyChar
  second <- anyChar
  third <- anyChar

  pure $ fromCharArray [ first, second, third ]

hetuParser :: Parser String Hetu
hetuParser = do
  rawDay <- twoDigitNum
  rawMonth <- twoDigitNum
  rawYear <- twoDigitNum
  decade <- parseDecade
  id <- parseId
  rawCheckSum <- anyChar

  let
    realCheckSum = checkSum rawDay rawMonth rawYear id
  eof
  if rawCheckSum == realCheckSum then
    pure
      $ { day: intToBottom rawDay
        , month: intToBottom rawMonth
        , twoYearDigits: rawYear
        , decade
        , id
        , checkSum: realCheckSum
        }
  else
    fail "Invalid checksum"

parseHetu :: String -> Either String Hetu
parseHetu input = case runParser input hetuParser of
  Left error -> Left $ parseErrorMessage error <> " at column " <> col
    where
    Position pos = parseErrorPosition error
    col = show pos.column
  Right success -> Right success
