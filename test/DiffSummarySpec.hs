module DiffSummarySpec where

import Test.Hspec

import Patch
import DiffSummary

testSummary :: DiffSummary String
testSummary = DiffSummary { description = "lol", patch = Insert DiffInfo, parentAnnotations = [] }

spec :: Spec
spec = parallel $ do
  describe "show" $ do
    it "should print adds" $
      show testSummary `shouldBe` "Added an 'a' expression"
