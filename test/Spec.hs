module Main where

import qualified Assigning.Assignment.Spec
import qualified Data.Functor.Classes.Ord.Generic.Spec
import qualified Data.Mergeable.Spec
import qualified Data.Source.Spec
import qualified Diffing.Algorithm.RWS.Spec
import qualified Diffing.Algorithm.SES.Spec
import qualified DiffSpec
import qualified InterpreterSpec
import qualified TermSpec
import qualified TOCSpec
import qualified IntegrationSpec
import qualified Semantic.CLI.Spec
import qualified Semantic.IO.Spec
import qualified Semantic.Stat.Spec
import qualified SemanticSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Semantic.Stat" Semantic.Stat.Spec.spec
  parallel $ do
    describe "Assigning.Assignment" Assigning.Assignment.Spec.spec
    describe "Data.Functor.Classes.Ord.Generic" Data.Functor.Classes.Ord.Generic.Spec.spec
    describe "Data.Mergeable" Data.Mergeable.Spec.spec
    describe "Data.Source" Data.Source.Spec.spec
    describe "Diff" DiffSpec.spec
    describe "Diffing.Algorithm.RWS" Diffing.Algorithm.RWS.Spec.spec
    describe "Diffing.Algorithm.SES" Diffing.Algorithm.SES.Spec.spec
    describe "Interpreter" InterpreterSpec.spec
    describe "Term" TermSpec.spec
    describe "Semantic" SemanticSpec.spec
    describe "Semantic.CLI" Semantic.CLI.Spec.spec
    describe "Semantic.IO" Semantic.IO.Spec.spec
    describe "TOC" TOCSpec.spec
    describe "Integration" IntegrationSpec.spec
