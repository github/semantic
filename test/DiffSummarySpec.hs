{-# LANGUAGE DataKinds #-}
module DiffSummarySpec where

import Prologue
import Data.Record
import Test.Hspec
import Diff
import Info
import Syntax
import Term
import Patch
import Range
import Category
import DiffSummary
import Text.PrettyPrint.Leijen.Text (pretty)
import Test.Hspec.QuickCheck
import Diff.Arbitrary
import Data.List (partition)

arrayInfo :: Info
arrayInfo = rangeAt 0 .: ArrayLiteral .: 2 .: 0 .: RNil

literalInfo :: Info
literalInfo = rangeAt 1 .: StringLiteral .: 1 .: 0 .: RNil

testDiff :: Diff Text Info
testDiff = free $ Free (pure arrayInfo :< Indexed [ free $ Pure (Insert (cofree $ literalInfo :< Leaf "a")) ])

testSummary :: DiffSummary DiffInfo
testSummary = DiffSummary { patch = Insert (LeafInfo "string" "a"), parentAnnotations = [] }

replacementSummary :: DiffSummary DiffInfo
replacementSummary = DiffSummary { patch = Replace (LeafInfo "string" "a") (LeafInfo "symbol" "b"), parentAnnotations = [ ArrayLiteral ] }

spec :: Spec
spec = parallel $ do
  describe "diffSummary" $ do
    it "outputs a diff summary" $ do
      diffSummary testDiff `shouldBe` [ DiffSummary { patch = Insert (LeafInfo "string" "a"), parentAnnotations = [ ArrayLiteral ] } ]
  describe "show" $ do
    it "should print adds" $
      show (pretty testSummary) `shouldBe` ("Added the 'a' string" :: Text)
    it "prints a replacement" $ do
      show (pretty replacementSummary) `shouldBe` ("Replaced the 'a' string with the 'b' symbol in the array context" :: Text)
  prop "patches in summaries match the patches in diffs" $
    \a -> let
      diff = (toDiff (a :: ArbitraryDiff Text (Record '[Category, Cost])))
      summaries = diffSummary diff
      patches = toList diff
      isIndexedOrFixed :: Patch (Term a annotation) -> Bool
      isIndexedOrFixed patch = case unwrap <$> patch of
        (Insert syntax) -> isIndexedOrFixed' syntax
        (Delete syntax) -> isIndexedOrFixed' syntax
        (Replace s1 s2) -> isIndexedOrFixed' s1 || isIndexedOrFixed' s2
      isIndexedOrFixed' syntax = case syntax of
        (Indexed _) -> True
        (Fixed _) -> True
        _ -> False
      isBranchInfo info = case info of
        (BranchInfo _ _ _) -> True
        (LeafInfo _ _) -> False
      isBranchNode :: DiffSummary DiffInfo -> Bool
      isBranchNode summary = (case patch summary of
        (Insert diffInfo) -> isBranchInfo diffInfo
        (Delete diffInfo) -> isBranchInfo diffInfo
        (Replace i1 i2) -> isBranchInfo i1 || isBranchInfo i2)
      in
        case (partition isBranchNode summaries, partition isIndexedOrFixed patches) of
          ((branchSummaries, otherSummaries), (branchPatches, otherPatches)) ->
            ((() <$) . patch <$> branchSummaries, (() <$) . patch <$> otherSummaries) `shouldBe` ((() <$) <$> branchPatches, (() <$) <$> otherPatches)
