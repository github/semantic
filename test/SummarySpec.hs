{-# LANGUAGE DataKinds #-}
module SummarySpec where

import Category
import Data.Functor.Both
import Data.Functor.Listable
import Data.List (partition)
import RWS
import Data.Record
import Data.String
import Diff
import Renderer.Summary
import Info
import Interpreter
import Patch
import Prologue
import Source
import SpecHelpers
import Syntax
import Term
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck
import Data.These

sourceSpanBetween :: (Int, Int) -> (Int, Int) -> SourceSpan
sourceSpanBetween (s1, e1) (s2, e2) = SourceSpan (SourcePos s1 e1) (SourcePos s2 e2)

arrayInfo :: Record '[Category, Range, SourceSpan]
arrayInfo = ArrayLiteral :. Range 0 3 :. sourceSpanBetween (1, 1) (1, 5) :. Nil

literalInfo :: Record '[Category, Range, SourceSpan]
literalInfo = StringLiteral :. Range 1 2 :. sourceSpanBetween (1, 2) (1, 4) :. Nil

testDiff :: Diff (Syntax Text) (Record '[Category, Range, SourceSpan])
testDiff = free $ Free (pure arrayInfo :< Indexed [ free $ Pure (Insert (cofree $ literalInfo :< Leaf "\"a\"")) ])

testSummary :: DiffSummary DiffInfo
testSummary = DiffSummary { diffSummaryPatch = Insert (LeafInfo StringLiteral "a" $ sourceSpanBetween (1,1) (1, 2)), parentAnnotation = [] }

replacementSummary :: DiffSummary DiffInfo
replacementSummary = DiffSummary { diffSummaryPatch = Replace (LeafInfo StringLiteral "a" $ sourceSpanBetween (1, 2) (1, 4)) (LeafInfo SymbolLiteral "b" $ sourceSpanBetween (1,1) (1, 2)), parentAnnotation = [Left (Info.FunctionCall, "foo")] }

blobs :: Both SourceBlob
blobs = both (SourceBlob (fromText "[]") nullOid "a.js" (Just defaultPlainBlob)) (SourceBlob (fromText "[a]") nullOid "b.js" (Just defaultPlainBlob))

spec :: Spec
spec = parallel $ do
  describe "diffSummaries" $ do
    it "outputs a diff summary" $
      diffSummaries blobs testDiff `shouldBe` [ JSONSummary "Added the \"a\" string" (SourceSpans . That $ sourceSpanBetween (1, 2) (1, 4)) ]

    prop "equal terms produce identity diffs" $
      \ a -> let term = defaultFeatureVectorDecorator (category . headF) (unListableF a :: SyntaxTerm String '[Category, Range, SourceSpan]) in
        diffSummaries blobs (diffTerms term term) `shouldBe` []

  describe "DiffInfo" $ do
    prop "patches in summaries match the patches in diffs" $
      \a -> let
        diff = unListableDiff a :: SyntaxDiff String '[Category, Range, SourceSpan]
        summaries = diffToDiffSummaries (source <$> blobs) diff
        patches = toList diff
        in
          case (partition isBranchNode (diffSummaryPatch <$> summaries), partition isIndexedOrFixed patches) of
            ((branchPatches, otherPatches), (branchDiffPatches, otherDiffPatches)) ->
              (() <$ branchPatches, () <$ otherPatches) `shouldBe` (() <$ branchDiffPatches, () <$ otherDiffPatches)
    prop "generates one LeafInfo for each child in an arbitrary branch patch" $
      \a -> let
        diff = unListableDiff a :: SyntaxDiff String '[Category, Range, SourceSpan]
        diffInfoPatches = diffSummaryPatch <$> diffToDiffSummaries (source <$> blobs) diff
        syntaxPatches = toList diff
        extractLeaves :: DiffInfo -> [DiffInfo]
        extractLeaves (BranchInfo children _ _) = join $ extractLeaves <$> children
        extractLeaves leaf = [ leaf ]

        extractDiffLeaves :: SyntaxTerm String '[Category, Range, SourceSpan] -> [ SyntaxTerm String '[Category, Range, SourceSpan] ]
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
                Prologue.length listOfLeaves `shouldBe` Prologue.length listOfDiffLeaves

isIndexedOrFixed :: Patch (Term (Syntax a) annotation) -> Bool
isIndexedOrFixed = any (isIndexedOrFixed' . unwrap)

isIndexedOrFixed' :: Syntax a f -> Bool
isIndexedOrFixed' syntax = case syntax of
  (Indexed _) -> True
  (Fixed _) -> True
  (Commented _ _) -> True
  _ -> False

isBranchNode :: Patch DiffInfo -> Bool
isBranchNode = any isBranchInfo
