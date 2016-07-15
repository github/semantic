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
  describe "DiffInfo" $ do
    prop "patches in summaries match the patches in diffs" $
      \a -> let
        diff = (toDiff (a :: ArbitraryDiff Text (Record '[Category, Cost])))
        summaries = diffSummary diff
        patches = toList diff
        in
          case (partition isBranchNode (patch <$> summaries), partition isIndexedOrFixed patches) of
            ((branchPatches, otherPatches), (branchDiffPatches, otherDiffPatches)) ->
              (() <$ branchPatches, () <$ otherPatches) `shouldBe` (() <$ branchDiffPatches, () <$ otherDiffPatches)
    prop "generates one LeafInfo for each child in an arbitrary branch patch" $
      \a -> let
        diff = (toDiff (a :: ArbitraryDiff Text (Record '[Category])))
        diffInfoPatches = patch <$> diffSummary diff
        syntaxPatches = toList diff
        extractLeaves :: DiffInfo -> [DiffInfo]
        extractLeaves (BranchInfo children _ _) = join $ extractLeaves <$> children
        extractLeaves leaf = [ leaf ]

        extractDiffLeaves :: Term Text (Record '[Category]) -> [ Term Text (Record '[Category]) ]
        extractDiffLeaves term = case unwrap term of
          (Indexed children) -> join $ extractDiffLeaves <$> children
          (Fixed children) -> join $ extractDiffLeaves <$> children
          Commented children leaf -> join $ extractDiffLeaves <$> children <> maybeToList leaf
          _ -> [ term ]
        in
          case (partition isBranchNode diffInfoPatches, partition isIndexedOrFixed syntaxPatches) of
            ((branchPatches, _), (diffPatches, _)) ->
              let listOfLeaves = foldMap extractLeaves (join $ toList <$> branchPatches)
                  listOfDiffLeaves = foldMap extractDiffLeaves (join $ toList <$> diffPatches)
               in
                length listOfLeaves `shouldBe` length listOfDiffLeaves

          -- partitions arbitrary diff infos ([BranchInfo], [LeafInfo])
          -- partitions arbitrary patches ([Fixed/Indexed], [Other])
          -- Map [BranchInfo] -> [LeafInfo]
          -- Map [Fixed/Indexed] -> [Children != Fixed/Indexed]
          -- length [Children != Fixed/Indexed] == length [LeafInfo]

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
isBranchNode :: Patch DiffInfo -> Bool
isBranchNode patch = case patch of
  (Insert diffInfo) -> isBranchInfo diffInfo
  (Delete diffInfo) -> isBranchInfo diffInfo
  (Replace i1 i2) -> isBranchInfo i1 || isBranchInfo i2