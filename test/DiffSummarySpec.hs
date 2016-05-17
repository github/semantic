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
testSummary = DiffSummary { patch = Insert (DiffInfo "a"), parentAnnotations = [] }

spec :: Spec
spec = parallel $ do
  describe "diffSummary" $ do
    it "outputs a diff summary" $ do
      diffSummary testDiff `shouldBe` [ DiffSummary { patch = Insert (DiffInfo "a"), parentAnnotations = [()] } ]
  describe "show" $ do
    it "should print adds" $
      show testSummary `shouldBe` "Added an 'a' expression"
