module Main where

import Prologue
import qualified IntegrationFormatSpec
import Test.Hspec

main :: IO ()
main = hspec $ parallel $ do
  describe "Integration Format Specs" IntegrationFormatSpec.spec
