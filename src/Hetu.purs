module Hetu (Hetu, parseHetu, formatHetu, isTemporary, gender, Gender(..)) where

import Prelude

import Data.Date (exactDate, year, day, month)
import Data.DateTime (Date, DateTime(..), Time(..), Year)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Int (even, fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.CodeUnits (fromCharArray, singleton)
import Partial.Unsafe (unsafeCrashWith)
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

realChecksum :: Date -> Int -> Char
realChecksum date id = case remToCheckSum $ mod (((dateToSixLetter date) * 1000) + id) 31 of
  -- There might be way to do this type safely without using "unsafeCrashWith"
  Left e -> unsafeCrashWith $ "failed to calculate checksum for date: " <> (show date) <> ", id: " <> (show id) <> ", " <> e
  Right r -> r

dateToSixLetter :: Date -> Int
dateToSixLetter date =
    fromEnum (day date) * 10000
  + fromEnum (month date) * 100
  + twoLastDigitsFromYear
  where
  yearNumeric = fromEnum (year date)
  twoLastDigitsFromYear = mod yearNumeric 100

-- | Render hetu in the traditional "ddmmyytnnnc" format.
formatHetu :: Hetu -> String
formatHetu hetu = process $ realChecksum hetu.birthday hetu.id
    where
    process checksum = date <> show century <> formatId hetu.id <> singleton checksum
    date = F.format (F.width 6 <> F.zeroFill) (dateToSixLetter hetu.birthday)
    century = centuryOf hetu

dateTimeFromHetu :: Date -> DateTime
dateTimeFromHetu d = DateTime d midnight
  where
  bottomEnum = fromMaybe bottom $ toEnum 0 :: forall a. BoundedEnum a => a
  midnight = Time bottomEnum bottomEnum bottomEnum bottomEnum

-- https://fi.wikipedia.org/wiki/Henkil%C3%B6tunnus#Tunnuksen_muoto
remToCheckSum :: Int -> Either String Char
remToCheckSum 0 = Right '0'
remToCheckSum 1 = Right '1'
remToCheckSum 2 = Right '2'
remToCheckSum 3 = Right '3'
remToCheckSum 4 = Right '4'
remToCheckSum 5 = Right '5'
remToCheckSum 6 = Right '6'
remToCheckSum 7 = Right '7'
remToCheckSum 8 = Right '8'
remToCheckSum 9 = Right '9'
remToCheckSum 10 = Right 'A'
remToCheckSum 11 = Right 'B'
remToCheckSum 12 = Right 'C'
remToCheckSum 13 = Right 'D'
remToCheckSum 14 = Right 'E'
remToCheckSum 15 = Right 'F'
remToCheckSum 16 = Right 'H'
remToCheckSum 17 = Right 'J'
remToCheckSum 18 = Right 'K'
remToCheckSum 19 = Right 'L'
remToCheckSum 20 = Right 'M'
remToCheckSum 21 = Right 'N'
remToCheckSum 22 = Right 'P'
remToCheckSum 23 = Right 'R'
remToCheckSum 24 = Right 'S'
remToCheckSum 25 = Right 'T'
remToCheckSum 26 = Right 'U'
remToCheckSum 27 = Right 'V'
remToCheckSum 28 = Right 'W'
remToCheckSum 29 = Right 'X'
remToCheckSum 30 = Right 'Y'
remToCheckSum remainder = Left $ "illegal checksum: " <> show remainder

twoDigitNum :: Parser String Int
twoDigitNum = do
  left <- digit
  right <- digit

  maybe twoDigitFailure pure $ charsToInt [ left, right ]
  where twoDigitFailure = fail "Cannot parse two part number"

charsToInt :: Array Char -> Maybe Int
charsToInt = fromString <<< fromCharArray

parseCentury :: Parser String HetuCentury
parseCentury = anyChar >>= parseCenturyChar
  where
  parseCenturyChar century =
    case century of
      '-' -> pure Minus
      '+' -> pure Plus
      'A' -> pure ALetter
      o -> fail $ "Invalid century: \"" <> singleton o <> "\""

parseNumId :: Parser String Int
parseNumId = parseId >>= verifyLegalId
  where
  -- "Yksilönumero on välillä 002–899. Numeroita 900–999 käytetään tilapäisissä
  -- henkilötunnuksissa"
  verifyLegalId id =
    if id < 2
    then
      fail "Id must be > 1"
    else
      pure id

parseId :: Parser String Int
parseId = do
  first <- digit
  second <- digit
  third <- digit

  maybe idFailure pure $ charsToInt [ first, second, third ]
  where
  idFailure = fail "Invalid id"

parseToDigitEnum :: forall a. BoundedEnum a => String -> Parser String a
parseToDigitEnum errorMessage = twoDigitNum >>= errorHandleDigits
  where
  digitFailure = fail errorMessage
  errorHandleDigits digits = maybe digitFailure pure $ toEnum digits

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

  let verifyChecksum actualCheckSum =
        if actualCheckSum == supposedCheckSum
        then
          pure { birthday, id }
        else
          fail "Invalid checksum"

  verifyChecksum (realChecksum birthday id)

-- | Parse and validate finnish national identification number "henkilötunnus".
parseHetu :: String -> Either String Hetu
parseHetu input = case runParser input hetuParser of
  Left error -> Left $ parseErrorMessage error <> " at column " <> col
    where
    Position pos = parseErrorPosition error
    col = show pos.column
  Right success -> Right success
