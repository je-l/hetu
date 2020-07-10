module Hetu (parseHetu, prettyPrintHetu, Hetu, isTemporary, gender, Gender(..)) where

import Prelude
import Data.Date (exactDate, year)
import Data.DateTime (Date, DateTime(..), Time(..), Year)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Int (even, fromString)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.String.CodeUnits (fromCharArray, singleton)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
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

-- | Valid hetu. Id is like an assigned serial number.
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
-- | https://fi.wikipedia.org/wiki/Henkil%C3%B6tunnus#Tunnuksen_muoto
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

realCheckSum :: Date -> Int -> Char
realCheckSum date id = remToCheckcsum $ mod combined 31
  where
  combined = unsafePartial (fromJust (fromString combinedMaybe))
  combinedMaybe = dateToSixLetter date <> formatId id

dateToSixLetter :: Date -> String
dateToSixLetter date = format hetuFormat datetime
  where
  hetuFormat = DayOfMonthTwoDigits : MonthTwoDigits : YearTwoDigits : Nil
  datetime = dateTimeFromHetu date

-- | Render hetu in the traditional "ddmmyytnnnc" format.
prettyPrintHetu :: Hetu -> String
prettyPrintHetu hetu = date <> show century <> formatId hetu.id <> singleton cs
  where
  date = dateToSixLetter hetu.birthday
  century = centuryOf hetu
  cs = realCheckSum hetu.birthday hetu.id

dateTimeFromHetu :: Date -> DateTime
dateTimeFromHetu d = DateTime d midnight
  where
  bottomEnum = fromMaybe bottom $ toEnum 0 :: forall a. BoundedEnum a => a
  midnight = Time bottomEnum bottomEnum bottomEnum bottomEnum

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
remToCheckcsum remainder =
  unsafeCrashWith $ "illegal checksum: " <> show remainder

twoDigitNum :: Parser String Int
twoDigitNum = do
  left <- digit
  right <- digit

  maybe
    (fail "Cannot parse two part number")
    pure $
    fromString (fromCharArray [ left, right ])

parseCentury :: Parser String HetuCentury
parseCentury = do
  c <- anyChar
  case c of
    '-' -> pure Minus
    '+' -> pure Plus
    'A' -> pure ALetter
    o -> fail $ "Invalid century: \"" <> singleton o <> "\""

parseId :: Parser String Int
parseId = do
  first <- digit
  second <- digit
  third <- digit

  let idNum = unsafePartial $
        fromJust (fromString (fromCharArray [ first, second, third ]))

  -- "Yksilönumero on välillä 002–899. Numeroita 900–999 käytetään tilapäisissä
  -- henkilötunnuksissa"
  if idNum < 2
  then
    fail "Too low id"
  else
    pure idNum

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
  id <- parseId
  supposedCheckSum <- anyChar
  eof

  if realCheckSum birthday id == supposedCheckSum
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
