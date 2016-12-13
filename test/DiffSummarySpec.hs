{-# LANGUAGE DataKinds #-}
module DiffSummarySpec where

import Category
import Data.Functor.Both
import Data.List (partition)
import Data.RandomWalkSimilarity
import Data.Record
import Diff
import Diff.Arbitrary
import DiffSummary
import Info
import Interpreter
import Patch
import Prologue
import Source
import Syntax
import Term
import Term.Arbitrary
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.QuickCheck
import Data.These
import Diffing (getLabel)

sourceSpanBetween :: (Int, Int) -> (Int, Int) -> SourceSpan
sourceSpanBetween (s1, e1) (s2, e2) = SourceSpan (SourcePos s1 e1) (SourcePos s2 e2)

arrayInfo :: Record '[Category, Range, SourceSpan]
arrayInfo = ArrayLiteral .: Range 0 3 .: sourceSpanBetween (1, 1) (1, 5) .: RNil

literalInfo :: Record '[Category, Range, SourceSpan]
literalInfo = StringLiteral .: Range 1 2 .: sourceSpanBetween (1, 2) (1, 4) .: RNil

testDiff :: Diff (Syntax Text) (Record '[Category, Range, SourceSpan])
testDiff = free $ Free (pure arrayInfo :< Indexed [ free $ Pure (Insert (cofree $ literalInfo :< Leaf "\"a\"")) ])

testSummary :: DiffSummary DiffInfo
testSummary = DiffSummary { patch = Insert (LeafInfo Category.StringLiteral "a" $ sourceSpanBetween (1,1) (1, 2)), parentAnnotation = [] }

replacementSummary :: DiffSummary DiffInfo
replacementSummary = DiffSummary { patch = Replace (LeafInfo Category.StringLiteral "a" $ sourceSpanBetween (1, 2) (1, 4)) (LeafInfo Category.SymbolLiteral "b" $ sourceSpanBetween (1,1) (1, 2)), parentAnnotation = [Left (Info.FunctionCall, "foo")] }

blobs :: Both SourceBlob
blobs = both (SourceBlob (fromText "[]") nullOid "a.js" (Just defaultPlainBlob)) (SourceBlob (fromText "[a]") nullOid "b.js" (Just defaultPlainBlob))

spec :: Spec
spec = parallel $ do
  describe "diffSummaries" $ do
    it "outputs a diff summary" $ do
      diffSummaries blobs testDiff `shouldBe` [ JSONSummary "Added the \"a\" string" (SourceSpans . That $ sourceSpanBetween (1, 2) (1, 4)) ]

    prop "equal terms produce identity diffs" $
      \ a -> let term = defaultFeatureVectorDecorator (category . headF) (toTerm (a :: ArbitraryTerm Text (Record '[Category, Range, SourceSpan]))) in
        diffSummaries blobs (diffTerms wrap (==) diffCost getLabel term term) `shouldBe` []

  describe "DiffInfo" $ do
    prop "patches in summaries match the patches in diffs" $
      \a -> let
        diff = (toDiff (a :: ArbitraryDiff Text (Record '[Category, Cost, Range, SourceSpan])))
        summaries = diffToDiffSummaries (source <$> blobs) diff
        patches = toList diff
        in
          case (partition isBranchNode (patch <$> summaries), partition isIndexedOrFixed patches) of
            ((branchPatches, otherPatches), (branchDiffPatches, otherDiffPatches)) ->
              (() <$ branchPatches, () <$ otherPatches) `shouldBe` (() <$ branchDiffPatches, () <$ otherDiffPatches)
    prop "generates one LeafInfo for each child in an arbitrary branch patch" $
      \a -> let
        diff = (toDiff (a :: ArbitraryDiff Text (Record '[Category, Range, SourceSpan])))
        diffInfoPatches = patch <$> diffToDiffSummaries (source <$> blobs) diff
        syntaxPatches = toList diff
        extractLeaves :: DiffInfo -> [DiffInfo]
        extractLeaves (BranchInfo children _ _) = join $ extractLeaves <$> children
        extractLeaves leaf = [ leaf ]

        extractDiffLeaves :: Term (Syntax Text) (Record '[Category, Range, SourceSpan]) -> [ Term (Syntax Text) (Record '[Category, Range, SourceSpan]) ]
        extractDiffLeaves term = case unwrap term of
          (Indexed children) -> join $ extractDiffLeaves <$> children
          (Fixed children) -> join $ extractDiffLeaves <$> children
          Commented children leaf -> children <> maybeToList leaf >>= extractDiffLeaves
          _ -> [ term ]
        in
          case (partition isBranchNode diffInfoPatches, partition isIndexedOrFixed syntaxPatches) of
            ((branchPatches, _), (diffPatches, _)) ->
              let listOfLeaves = foldMap extractLeaves (join $ toList <$> branchPatches)
                  listOfDiffLeaves = foldMap extractDiffLeaves (diffPatches >>= toList)
               in
                length listOfLeaves `shouldBe` length listOfDiffLeaves

isIndexedOrFixed :: Patch (Term (Syntax a) annotation) -> Bool
isIndexedOrFixed = any (isIndexedOrFixed' . unwrap)

isIndexedOrFixed' :: Syntax a f -> Bool
isIndexedOrFixed' syntax = case syntax of
  (Indexed _) -> True
  (Fixed _) -> True
  _ -> False

isBranchNode :: Patch DiffInfo -> Bool
isBranchNode = any isBranchInfo
