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
import Interpreter
import Diff.Arbitrary
import Text.Megaparsec.Text
import Text.Megaparsec
import Data.List (partition)

arrayInfo :: Info
arrayInfo = rangeAt 0 .: ArrayLiteral .: 2 .: 0 .: RNil

literalInfo :: Info
literalInfo = rangeAt 1 .: StringLiteral .: 1 .: 0 .: RNil

testDiff :: Diff Text Info
testDiff = free $ Free (pure arrayInfo :< Indexed [ free $ Pure (Insert (cofree $ literalInfo :< Leaf "a")) ])

testSummary :: DiffSummary DiffInfo
testSummary = DiffSummary { patch = Insert (DiffInfo "string" "a"), parentAnnotations = [], patchAnnotations = [] }

replacementSummary :: DiffSummary DiffInfo
replacementSummary = DiffSummary { patch = Replace (DiffInfo "string" "a") (DiffInfo "symbol" "b"), parentAnnotations = [ ArrayLiteral ], patchAnnotations = [] }

spec :: Spec
spec = parallel $ do
  describe "diffSummary" $ do
    it "outputs a diff summary" $ do
      diffSummary testDiff `shouldBe` [ DiffSummary { patch = Insert (DiffInfo "string" "a"), parentAnnotations = [ ArrayLiteral ], patchAnnotations = [] } ]
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
      isBranchNode :: DiffSummary DiffInfo -> Bool
      isBranchNode summary = (not . null $ patchAnnotations summary) || (case patch summary of
        (Insert diffInfo) -> termName diffInfo == "branch"
        (Delete diffInfo) -> termName diffInfo == "branch"
        (Replace i1 i2) -> termName i1 == "branch" || termName i2 == "branch")
      in
        case (partition isBranchNode summaries, partition isIndexedOrFixed patches) of
          ((branchSummaries, otherSummaries), (branchPatches, otherPatches)) ->
            ((() <$) . patch <$> branchSummaries, (() <$) . patch <$> otherSummaries) `shouldBe` ((() <$) <$> branchPatches, (() <$) <$> otherPatches)

        -- ((() <$) <$> (patch <$> summaries)) `shouldBe` ((() <$) <$> patches)
        -- [Insert (), Insert ()] == [ Insert () ]
        -- explodePatch :: Patch (Syntax a) -> [Patch (Syntax a)]
        -- explodePatch Indexed = explodePatch <$> children

        -- Patches of branch nodes with children should have a summary for each child that is not a branch node
        -- Patches of branch nodes with children that are branch nodes shoudl have a summary for each of those children or one summary per branch if the branches are empty
-- let xs = ArbitraryPure (Insert (ArbitraryTerm {annotation = Category.Operator .: RNil, syntax = Indexed [ArbitraryTerm {annotation = Program .: RNil, syntax = Leaf ""}]}))
-- let xs = ArbitraryPure (Delete (ArbitraryTerm {annotation = Category.Case .: RNil, syntax = Fixed [ArbitraryTerm {annotation = Category.FunctionCall .: RNil, syntax = Leaf ""}]})) :: ArbitraryDiff Text (Record '[Category])
          -- ((() <$) . patch <$> summaries) `shouldBe` ((() <$) <$> otherPatches)
