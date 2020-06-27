module Main where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, (!!))
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Unsafe (char)
import Partial.Unsafe (unsafePartial)

type Hetu =
  {
    year :: Int
  }

baseFormat :: Regex
baseFormat = unsafeRegex "^(\\d{2})(\\d{2})(\\d{2})([+-A])(\\d{3})(.)$" noFlags

parseYear :: Int -> Char -> Either String Int
parseYear yearDigits '+' = Right (1800 + yearDigits)
parseYear yearDigits '-' = Right (1900 + yearDigits)
parseYear yearDigits 'A' = Right (2000 + yearDigits)
parseYear yearDigits c = Left ("unknown decade identifier: " <> (show c))

doubleMaybe :: forall a. Partial => Maybe (Maybe a) -> a
doubleMaybe = fromJust <<< fromJust

interpretYear :: (NonEmptyArray (Maybe String)) -> Either String Hetu
interpretYear inp = case rawYear of
  Left err -> Left err
  Right year -> Right { year }
  where rawYear = parseYear y decade
        y = unsafePartial (fromJust (fromString (doubleMaybe (inp !! 3))))
        decade = unsafePartial (char (doubleMaybe (inp !! 4)))

parseHetu :: String -> Either String Hetu
parseHetu input = case match baseFormat input of
  Nothing -> Left "doesnt match"
  Just a -> interpretYear a
