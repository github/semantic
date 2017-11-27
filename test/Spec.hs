module Main where

import qualified Assigning.Assignment.Spec
import qualified CommandSpec
import qualified Data.Functor.Classes.Ord.Generic.Spec
import qualified Data.Mergeable.Spec
import qualified Diffing.Algorithm.RWS.Spec
import qualified DiffSpec
import qualified InterpreterSpec
import qualified SES.Spec
import qualified SourceSpec
import qualified TermSpec
import qualified TOCSpec
import qualified IntegrationSpec
import qualified SemanticCmdLineSpec
import qualified SemanticSpec
import qualified Semantic.StatSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Semantic.Stat" Semantic.StatSpec.spec
  parallel $ do
    describe "Assigning.Assignment" Assigning.Assignment.Spec.spec
    describe "Command" CommandSpec.spec
    describe "Data.Functor.Classes.Ord.Generic" Data.Functor.Classes.Ord.Generic.Spec.spec
    describe "Data.Mergeable" Data.Mergeable.Spec.spec
    describe "Diff" DiffSpec.spec
    describe "Diffing.Algorithm.RWS" Diffing.Algorithm.RWS.Spec.spec
    describe "Interpreter" InterpreterSpec.spec
    describe "SES" SES.Spec.spec
    describe "Source" SourceSpec.spec
    describe "Term" TermSpec.spec
    describe "Semantic" SemanticSpec.spec
    describe "SemanticCmdLine" SemanticCmdLineSpec.spec
    describe "TOC" TOCSpec.spec
    describe "Integration" IntegrationSpec.spec
