module DiffSummarySpec where

import Test.Hspec
import Diff
import Info
import Syntax
import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free
import Patch
import Range
import Category
import Data.Set
import DiffSummary

arrayInfo :: Info
arrayInfo = Info (rangeAt 0) (singleton ArrayLiteral) 2

literalInfo :: Info
literalInfo = Info (rangeAt 1) (singleton StringLiteral) 1

testDiff :: Diff String Info
testDiff = free $ Free (pure arrayInfo :< Indexed [ free $ Pure (Insert (cofree $ literalInfo :< Leaf "a")) ])

testSummary :: DiffSummary Char
testSummary = DiffSummary { patch = Insert (DiffInfo "string literal" (Just "a")), parentAnnotations = [] }

replacementSummary :: DiffSummary Char
replacementSummary = DiffSummary { patch = Replace (DiffInfo "string literal" (Just "a")) (DiffInfo "symbol literal" (Just "b")), parentAnnotations = [ (DiffInfo "array literal" Nothing)] }

spec :: Spec
spec = parallel $ do
  describe "diffSummary" $ do
    it "outputs a diff summary" $ do
      diffSummary testDiff `shouldBe` [ DiffSummary { patch = Insert (DiffInfo "string literal" (Just "a")), parentAnnotations = [ DiffInfo "array literal" Nothing ] } ]
  describe "show" $ do
    it "should print adds" $
      show testSummary `shouldBe` "Added the 'a' string literal"
    it "prints a replacement" $ do
      show replacementSummary `shouldBe` "Replaced the 'a' string literal with the 'b' symbol literal in the array literal context"
