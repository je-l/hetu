module Test.Main where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.String (null)
import Data.String.Utils (lines)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Main (Hetu, parseHetu, prettyPrintHetu)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

unsafeParseHetu :: String -> Hetu
unsafeParseHetu input = case parseHetu input of
  Left e -> unsafeCrashWith $ "parse failed for valid hetu \"" <> input <> "\": " <> show e
  Right u -> u

compareHetus :: String -> Aff Unit
compareHetus hetuInput = prettyPrintHetu stringHetu `shouldEqual` hetuInput
  where stringHetu = unsafeParseHetu hetuInput

type InvalidHetu = { input :: String, error :: String }

invalidHetus :: List InvalidHetu
invalidHetus =
  (
    { input: "aaaa", error: "Expected digit at column 1"} :
    { input: "280264-051E", error: "Invalid checksum at column 12"} :
    Nil
  )

compareInvalidHetus :: InvalidHetu -> Aff Unit
compareInvalidHetus hetu = case parseHetu hetu.input of
  Right success -> unsafeCrashWith $
    "expected parse error but it was successful: " <> hetu.input
  Left err -> err `shouldEqual` hetu.error

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "hetu parsing" do
    it "parse valid hetus" do
      dumpFile <- readTextFile UTF8 "generated_hetus.txt"
      let nonemptyLines = filter (not <<< null) $ lines dumpFile
      traverse_ compareHetus nonemptyLines

    it "should give informative errors for failing hetus" do
      traverse_ compareInvalidHetus invalidHetus
