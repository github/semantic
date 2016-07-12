module Main where

import Prologue
import qualified AlignmentSpec
import qualified CorpusSpec
import qualified Data.RandomWalkSimilarity.Spec
import qualified Diff.Spec
import qualified DiffSummarySpec
import qualified InterpreterSpec
import qualified OrderedMapSpec
import qualified PatchOutputSpec
import qualified TermSpec
import Test.Hspec

main :: IO ()
main = hspec . parallel $ do
  describe "Alignment" AlignmentSpec.spec
  describe "Corpus" CorpusSpec.spec
  describe "Data.RandomWalkSimilarity" Data.RandomWalkSimilarity.Spec.spec
  describe "Diff.Spec" Diff.Spec.spec
  describe "DiffSummary" DiffSummarySpec.spec
  describe "Interpreter" InterpreterSpec.spec
  describe "OrderedMap" OrderedMapSpec.spec
  describe "PatchOutput" PatchOutputSpec.spec
  describe "Term" TermSpec.spec
