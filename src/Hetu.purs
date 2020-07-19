module Hetu (Hetu, parseHetu, prettyPrintHetu, isTemporary, gender, Gender(..)) where

import Prelude

import Data.Date (exactDate, year)
import Data.DateTime (Date, DateTime(..), Time(..), Year)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Int (even, fromString)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.CodeUnits (fromCharArray, singleton)
import Text.Format as F
import Text.Parsing.Parser (Parser, fail, parseErrorMessage, parseErrorPosition, runParser)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (anyChar, eof)
import Text.Parsing.Parser.Token (digit)

data HetuCentury = Minus | Plus | ALetter

instance showCentury :: Show HetuCentury where
  show Plus = "+"
  show Minus = "-"
  show ALetter = "A"

-- | Valid hetu. Id is kind of like a serial number.
type Hetu =
  { birthday :: Date
  , id :: Int
  }

data Gender = Male | Female

instance showGender :: Show Gender where
  show Male = "Male"
  show Female = "Female"

derive instance eqGender :: Eq Gender

-- | Even hetu id is given for females and odd for males.
gender :: Hetu -> Gender
gender hetu = if even hetu.id then Female else Male

-- | Hetus with id >= 900 are considered "temporary". See
-- | https://en.wikipedia.org/wiki/National_identification_number#Finland
isTemporary :: Hetu -> Boolean
isTemporary hetu = hetu.id >= 900

formatId :: Int -> String
formatId = F.format $ F.width 3 <> F.zeroFill

yearFromHetu :: Int -> HetuCentury -> Maybe Year
yearFromHetu hetu letter = toEnum cumulativeYear
  where cumulativeYear = case letter of
          Minus -> 1900 + hetu
          Plus -> 1800 + hetu
          ALetter -> 2000 + hetu

centuryOf :: Hetu -> HetuCentury
centuryOf hetu = intYear hetu.birthday
  where
  intYear = yearToCentury <<< fromEnum <<< year
  yearToCentury y
    | y < 1900 = Plus
    | y >= 2000 = ALetter
    | otherwise = Minus

realCheckSum :: Date -> Int -> Either String Char
realCheckSum date id = case fromString $ dateToSixLetter date <> formatId id of
  Nothing -> Left "Invalid date for checksum"
  Just combined -> case remToCheckcsum $ mod combined 31 of
    Left e -> Left $ "Error calculating checksum: " <> e
    Right r -> Right r

dateToSixLetter :: Date -> String
dateToSixLetter date = format hetuFormat datetime
  where
  hetuFormat = DayOfMonthTwoDigits : MonthTwoDigits : YearTwoDigits : Nil
  datetime = dateTimeFromHetu date

-- | Render hetu in the traditional "ddmmyytnnnc" format.
prettyPrintHetu :: Hetu -> Either String String
prettyPrintHetu hetu = case realCheckSum hetu.birthday hetu.id of
  Left e -> Left e
  Right cs -> Right $ date <> show century <> formatId hetu.id <> singleton cs
    where
    date = dateToSixLetter hetu.birthday
    century = centuryOf hetu

dateTimeFromHetu :: Date -> DateTime
dateTimeFromHetu d = DateTime d midnight
  where
  bottomEnum = fromMaybe bottom $ toEnum 0 :: forall a. BoundedEnum a => a
  midnight = Time bottomEnum bottomEnum bottomEnum bottomEnum

-- https://fi.wikipedia.org/wiki/Henkil%C3%B6tunnus#Tunnuksen_muoto
remToCheckcsum :: Int -> Either String Char
remToCheckcsum 0 = Right '0'
remToCheckcsum 1 = Right '1'
remToCheckcsum 2 = Right '2'
remToCheckcsum 3 = Right '3'
remToCheckcsum 4 = Right '4'
remToCheckcsum 5 = Right '5'
remToCheckcsum 6 = Right '6'
remToCheckcsum 7 = Right '7'
remToCheckcsum 8 = Right '8'
remToCheckcsum 9 = Right '9'
remToCheckcsum 10 = Right 'A'
remToCheckcsum 11 = Right 'B'
remToCheckcsum 12 = Right 'C'
remToCheckcsum 13 = Right 'D'
remToCheckcsum 14 = Right 'E'
remToCheckcsum 15 = Right 'F'
remToCheckcsum 16 = Right 'H'
remToCheckcsum 17 = Right 'J'
remToCheckcsum 18 = Right 'K'
remToCheckcsum 19 = Right 'L'
remToCheckcsum 20 = Right 'M'
remToCheckcsum 21 = Right 'N'
remToCheckcsum 22 = Right 'P'
remToCheckcsum 23 = Right 'R'
remToCheckcsum 24 = Right 'S'
remToCheckcsum 25 = Right 'T'
remToCheckcsum 26 = Right 'U'
remToCheckcsum 27 = Right 'V'
remToCheckcsum 28 = Right 'W'
remToCheckcsum 29 = Right 'X'
remToCheckcsum 30 = Right 'Y'
remToCheckcsum remainder = Left $ "illegal checksum: " <> show remainder

twoDigitNum :: Parser String Int
twoDigitNum = do
  left <- digit
  right <- digit

  maybe
    (fail "Cannot parse two part number")
    pure $
    charsToInt [ left, right ]

charsToInt :: Array Char -> Maybe Int
charsToInt = fromString <<< fromCharArray

parseCentury :: Parser String HetuCentury
parseCentury = do
  c <- anyChar
  case c of
    '-' -> pure Minus
    '+' -> pure Plus
    'A' -> pure ALetter
    o -> fail $ "Invalid century: \"" <> singleton o <> "\""

parseNumId :: Parser String Int
parseNumId = do
  id <- parseId

  -- "Yksilönumero on välillä 002–899. Numeroita 900–999 käytetään tilapäisissä
  -- henkilötunnuksissa"
  if id < 2
  then
    fail "Too low id"
  else
    pure id

parseId :: Parser String Int
parseId = do
  first <- digit
  second <- digit
  third <- digit

  maybe (fail "Invalid id") pure (charsToInt [ first, second, third ])

parseToDigitEnum :: forall a. BoundedEnum a => String -> Parser String a
parseToDigitEnum error = do
  digits <- twoDigitNum
  maybe (fail error) pure (toEnum digits)

parseDate :: Parser String Date
parseDate = do
  day <- parseToDigitEnum "Illegal day"
  month <- parseToDigitEnum "Illegal month"
  rawYear <- twoDigitNum
  century <- parseCentury

  case yearFromHetu rawYear century of
    Nothing -> fail "Illegal year"
    Just year -> maybe
      (fail "Illegal date")
      pure $
      exactDate year month day

hetuParser :: Parser String Hetu
hetuParser = do
  birthday <- parseDate
  id <- parseNumId
  supposedCheckSum <- anyChar
  eof

  case realCheckSum birthday id of
    Left e -> fail $ "Checksum error: " <> e
    Right sum ->
      if sum == supposedCheckSum
      then
        pure { birthday, id }
      else
        fail "Invalid checksum"

-- | Parse and validate finnish national identification number "henkilötunnus".
parseHetu :: String -> Either String Hetu
parseHetu input = case runParser input hetuParser of
  Left error -> Left $ parseErrorMessage error <> " at column " <> col
    where
    Position pos = parseErrorPosition error
    col = show pos.column
  Right success -> Right success
