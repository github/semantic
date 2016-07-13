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
import Term.Arbitrary
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
    \a b -> let
      diff = diffTerms wrap (==) diffCost (toTerm a) (toTerm (b :: ArbitraryTerm Text Info))
      prettyDiff = show . pretty $ diffSummary diff in
        parsePrettyDiff prettyDiff `shouldBe` Just (diffSummary diff)

parsePrettyDiff :: Text -> Maybe [DiffSummary DiffInfo]
parsePrettyDiff string = parseMaybe diffParser string

parsePatch :: Parsec Text (Patch Text)
parsePatch = (\x y z -> case x of
  "Added" -> Insert (toS z)
  "Deleted" -> Delete (toS z)) <$> (string "Added" <|> string "Deleted") <*> (space *> string "the" <* space) <*> between (char '\'') (char '\'') (many printChar)

diffParser :: Parsec Text (DiffSummary DiffInfo)
diffParser = do
  patch <- parsePatch
  annotations <- _
  pure $ DiffSummary patch annotations