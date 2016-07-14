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
      show (pretty testSummary) `shouldBe` ("Added the 'a' string" :: Text)
    it "prints a replacement" $ do
      show (pretty replacementSummary) `shouldBe` ("Replaced the 'a' string with the 'b' symbol in the array context" :: Text)
  prop "diff summaries of arbitrary diffs are identical" $
    \a -> let
      diff = (toDiff (a :: ArbitraryDiff Text (Record '[Category])))
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
      isBranchCategory syntax = case syntax of; (Insert info) -> termName info == "branch" || categoryName info `elem` ["Indexed", "Fixed"]; (Delete info) -> termName info == "branch" || categoryName info `elem` ["Indexed", "Fixed"]; (Replace i1 i2) -> termName i1 == "branch" || categoryName i1 `elem` ["Indexed", "Fixed"] || categoryName i1 `elem` ["Indexed", "Fixed"] || termName i2  == "branch";
      in
        case (partition isBranchCategory (patch <$> summaries), partition isIndexedOrFixed patches) of
          ((branchSummaries, otherSummaries), (branchPatches, otherPatches)) ->
            (() <$ branchSummaries, () <$ otherSummaries) `shouldBe` (() <$ branchPatches, () <$ otherPatches)

          -- ((() <$) . patch <$> summaries) `shouldBe` ((() <$) <$> otherPatches)


parsePrettyDiff :: Text -> Maybe [DiffSummary DiffInfo]
parsePrettyDiff string = parseMaybe diffParser string

parsePatch :: Parsec Text (Patch DiffInfo)
parsePatch = (\x y z a -> case x of
  "Added" -> Insert (DiffInfo (toS z) (toS a))
  "Deleted" -> Delete (DiffInfo(toS z) (toS a))) <$> (string "Added" <|> string "Deleted") <*> (space *> string "the" <* space) <*> between (char '\'') (char '\'') (many printChar) <*> (space *> many printChar)

diffParser :: Parsec Text [(DiffSummary DiffInfo)]
diffParser = (DiffSummary <$> parsePatch <*> pure []) `sepBy` newline
