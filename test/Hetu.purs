module Test.Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (filter)
import Data.Either (Either(..), either)
import Data.String (null)
import Data.String.Utils (lines)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, Error, error, launchAff_, throwError)
import Hetu (Gender(..), Hetu, gender, isTemporary, parseHetu, prettyPrintHetu)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)


throwParseHetu :: forall a. MonadThrow Error a => String -> a Hetu
throwParseHetu hetu = either fail pure (parseHetu hetu)
  where
  msg err = error $ "Failed to parse hetu which should have succeeded: " <> err
  fail e = throwError $ msg e

compareHetus :: String -> Aff Unit
compareHetus hetuInput = do
  hetu <- throwParseHetu hetuInput
  hetuInput `shouldEqual` prettyPrintHetu hetu

assertParseFails :: String -> String -> Aff Unit
assertParseFails hetu expectedError = case parseHetu hetu of
  Right success -> fail $
    "expected error but parse succeeded: " <> hetu
  Left actualError -> shouldEqual expectedError actualError

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "successful hetu parsing" do
    it "parse valid hetus" do
      dumpFile <- readTextFile UTF8 "fake_valid_hetus.txt"
      let nonemptyLines = filter (not <<< null) $ lines dumpFile
      traverse_ compareHetus nonemptyLines

  describe "parsing invalid hetus" do
    it "should fail with too short hetu" do
      assertParseFails "aaaa" "Expected digit at column 1"

    it "should fail with invalid checksum" do
      assertParseFails "280264-051E" "Invalid checksum at column 12"

    it "should fail with too large day" do
      assertParseFails "320264-051U" "Illegal day at column 3"

    it "should fail with illegal date" do
      assertParseFails "310464-051U" "Illegal date at column 8"

    it "should fail with invalid century character" do
      assertParseFails "131052B308T" "Invalid century: \"B\" at column 8"

    it "should fail with too long hetu" do
      assertParseFails "311215A002JJ" "Expected EOF at column 12"

    it "should fail to parse too large day within a leap year" do
      assertParseFails "290200-101P" "Illegal date at column 8"

    it "should fail to parse day with alphabets" do
      assertParseFails "0101AA-123A" "Expected digit at column 5"

    -- "Yksilönumero on välillä 002–899. Numeroita 900–999 käytetään
    -- tilapäisissä henkilötunnuksissa"
    it "should fail to parse hetu with too low id" do
      assertParseFails "311215A001J" "Too low id at column 11"

  describe "hetu properties" do
    it "should infer correct gender" do
      maleHetu <- throwParseHetu "280264-051U"
      gender maleHetu `shouldEqual` Male

      femaleHetu <- throwParseHetu "010199-8148"
      gender femaleHetu `shouldEqual` Female

    it "should infer temporary status for hetu with id 900" do
      temporary <- throwParseHetu "020288+9818"
      isTemporary temporary `shouldEqual` true

      regular <- throwParseHetu "131052-308T"
      isTemporary regular `shouldEqual` false
