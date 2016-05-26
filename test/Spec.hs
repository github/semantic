module Main where

import Prologue
import qualified AlignmentSpec
import qualified CorpusSpec
import qualified Data.Adjoined.Spec
import qualified Data.Functor.Both.Spec
import qualified InterpreterSpec
import qualified OrderedMapSpec
import qualified PatchOutputSpec
import qualified TermSpec
import Test.Hspec

main :: IO ()
main = hspec $ parallel $ do
  describe "Alignment" AlignmentSpec.spec
  describe "Corpus" CorpusSpec.spec
  describe "Data.Adjoined" Data.Adjoined.Spec.spec
  describe "Data.Functor.Both" Data.Functor.Both.Spec.spec
  describe "Interpreter" InterpreterSpec.spec
  describe "OrderedMap" OrderedMapSpec.spec
  describe "PatchOutput" PatchOutputSpec.spec
  describe "Term" TermSpec.spec
