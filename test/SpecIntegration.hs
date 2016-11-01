module Main where

import Prologue
import qualified IntegrationFormatSpec
import Test.Hspec

main :: IO ()
main = hspec . parallel $ describe "Integration Format Specs" IntegrationFormatSpec.spec
