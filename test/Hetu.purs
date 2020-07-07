module Test.Main where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Data.String (null)
import Data.String.Utils (lines)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Hetu (parseHetu, prettyPrintHetu)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

compareHetus :: String -> Aff Unit
compareHetus hetuInput = case parseHetu hetuInput of
    Left e -> fail $ "failed to parse valid hetu: " <> e
    Right validHetu -> hetuInput `shouldEqual` prettyPrintHetu validHetu

assertParseFails :: String -> String -> Aff Unit
assertParseFails hetu expectedError = case parseHetu hetu of
  Right success -> fail $
    "expected parse error but it was successful: " <> hetu
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
      assertParseFails "310464-051U" "Illegal date combination at column 8"

    it "should fail with invalid century character" do
      assertParseFails "131052B308T" "Invalid century: \"B\" at column 8"
