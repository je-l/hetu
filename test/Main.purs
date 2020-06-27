module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (parseHetu)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "hetu parsing" do
    it "should parse valid hetu" do
      let hetu = parseHetu "230850-3970"
      hetu `shouldEqual` (Right { year: 1950 })

    it "should give informative error for invalid hetu" do
      let invalidHetu = parseHetu "1234567-123A"
      invalidHetu `shouldEqual` (Left "doesnt match")
