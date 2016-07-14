{-# LANGUAGE DataKinds #-}
module DiffSummarySpec where

import Prologue
import Data.Record
import Test.Hspec
import Diff
import Info
import Syntax
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
      summaries = diffSummary diff in
        ((() <$) . patch <$> summaries) `shouldBe` ((() <$) <$> toList diff) 

parsePrettyDiff :: Text -> Maybe [DiffSummary DiffInfo]
parsePrettyDiff string = parseMaybe diffParser string

parsePatch :: Parsec Text (Patch DiffInfo)
parsePatch = (\x y z a -> case x of
  "Added" -> Insert (DiffInfo (toS z) (toS a))
  "Deleted" -> Delete (DiffInfo(toS z) (toS a))) <$> (string "Added" <|> string "Deleted") <*> (space *> string "the" <* space) <*> between (char '\'') (char '\'') (many printChar) <*> (space *> many printChar)

diffParser :: Parsec Text [(DiffSummary DiffInfo)]
diffParser = (DiffSummary <$> parsePatch <*> pure []) `sepBy` newline
