module DiffSummarySpec where

import Prologue
import Data.String
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

testDiff :: Diff String Info
testDiff = free $ Free (pure arrayInfo :< Indexed [ free $ Pure (Insert (cofree $ literalInfo :< Leaf "a")) ])

testSummary :: DiffSummary DiffInfo
testSummary = DiffSummary { patch = Insert (DiffInfo "string" (Just "a")), parentAnnotations = [] }

replacementSummary :: DiffSummary DiffInfo
replacementSummary = DiffSummary { patch = Replace (DiffInfo "string" (Just "a")) (DiffInfo "symbol" (Just "b")), parentAnnotations = [ (DiffInfo "array" (Just "switch {}")) ] }

spec :: Spec
spec = parallel $ do
  describe "diffSummary" $ do
    it "outputs a diff summary" $ do
      diffSummary testDiff `shouldBe` [ DiffSummary { patch = Insert (DiffInfo "string" (Just "a")), parentAnnotations = [ DiffInfo "array" Nothing ] } ]
  describe "show" $ do
    it "should print adds" $
      show testSummary `shouldBe` ("Added the 'a' string" :: String)
    it "prints a replacement" $ do
      show replacementSummary `shouldBe` ("Replaced the 'a' string with the 'b' symbol in the array context" :: String)
