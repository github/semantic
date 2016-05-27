module Main where

import Prologue
import qualified AlignmentSpec
import qualified CorpusSpec
import qualified InterpreterSpec
import qualified OrderedMapSpec
import qualified PatchOutputSpec
import qualified TermSpec
import qualified DiffSummarySpec
import Test.Hspec

main :: IO ()
main = hspec $ parallel $ do
  describe "Alignment" AlignmentSpec.spec
  describe "Corpus" CorpusSpec.spec
  describe "Interpreter" InterpreterSpec.spec
  describe "OrderedMap" OrderedMapSpec.spec
  describe "PatchOutput" PatchOutputSpec.spec
  describe "Term" TermSpec.spec
  describe "DiffSummary" DiffSummarySpec.spec
