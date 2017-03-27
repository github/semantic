module Main where

import Prologue
import qualified AlignmentSpec
import qualified Data.Mergeable.Spec
import qualified Data.RandomWalkSimilarity.Spec
import qualified DiffSpec
import qualified SummarySpec
import qualified InterpreterSpec
import qualified PatchOutputSpec
import qualified RangeSpec
import qualified SES.Myers.Spec
import qualified SourceSpec
import qualified TermSpec
import qualified TOCSpec
import qualified DiffCommandSpec
import qualified ParseCommandSpec
import qualified IntegrationSpec
import Test.Hspec

main :: IO ()
main = hspec . parallel $ do
  describe "Alignment" AlignmentSpec.spec
  describe "Data.Mergeable" Data.Mergeable.Spec.spec
  describe "Data.RandomWalkSimilarity" Data.RandomWalkSimilarity.Spec.spec
  describe "Diff" DiffSpec.spec
  describe "Summary" SummarySpec.spec
  describe "Interpreter" InterpreterSpec.spec
  describe "PatchOutput" PatchOutputSpec.spec
  describe "Range" RangeSpec.spec
  describe "SES.Myers" SES.Myers.Spec.spec
  describe "Source" SourceSpec.spec
  describe "Term" TermSpec.spec
  describe "TOC" TOCSpec.spec
  describe "DiffCommand" DiffCommandSpec.spec
  describe "ParseCommand" ParseCommandSpec.spec
  describe "Integration" IntegrationSpec.spec
