module DiffSummarySpec where

import Prologue
import Test.Hspec
import Diff
import Info
import Syntax
import Patch
import Range
import Category
import DiffSummary

arrayInfo :: Info
arrayInfo = Info (rangeAt 0) ArrayLiteral 2 0

literalInfo :: Info
literalInfo = Info (rangeAt 1) StringLiteral 1 0

testDiff :: Diff Text Info
testDiff = free $ Free (pure arrayInfo :< Indexed [ free $ Pure (Insert (cofree $ literalInfo :< Leaf "a")) ])

testSummary :: DiffSummary DiffInfo
testSummary = DiffSummary { patch = Insert (DiffInfo "string" "a"), parentAnnotations = [] }

replacementSummary :: DiffSummary DiffInfo
replacementSummary = DiffSummary { patch = Replace (DiffInfo "string" "a") (DiffInfo "symbol" "b"), parentAnnotations = [ ArrayLiteral ] }

spec :: Spec
spec = parallel $ do
  describe "diffSummary" $ do
    it "outputs a diff summary" $ do
      diffSummary testDiff `shouldBe` [ DiffSummary { patch = Insert (DiffInfo "string" "a"), parentAnnotations = [ ArrayLiteral ] } ]
  describe "show" $ do
    it "should print adds" $
      show testSummary `shouldBe` ("Added the 'a' string" :: Text)
    it "prints a replacement" $ do
      show replacementSummary `shouldBe` ("Replaced the 'a' string with the 'b' symbol in the array context" :: Text)
