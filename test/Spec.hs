module Main where

import qualified Analysis.Python.Spec
import qualified Assigning.Assignment.Spec
import qualified Data.Diff.Spec
import qualified Data.Functor.Classes.Generic.Spec
import qualified Data.Mergeable.Spec
import qualified Data.Source.Spec
import qualified Data.Term.Spec
import qualified Diffing.Algorithm.RWS.Spec
import qualified Diffing.Algorithm.SES.Spec
import qualified Diffing.Interpreter.Spec
import qualified Integration.Spec
import qualified Rendering.TOC.Spec
import qualified Rendering.Imports.Spec
import qualified Semantic.Spec
import qualified Semantic.CLI.Spec
import qualified Semantic.IO.Spec
import qualified Semantic.Stat.Spec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Semantic.Stat" Semantic.Stat.Spec.spec
  parallel $ do
    describe "Analysis.Python" Analysis.Python.Spec.spec
    describe "Assigning.Assignment" Assigning.Assignment.Spec.spec
    describe "Data.Diff" Data.Diff.Spec.spec
    describe "Data.Functor.Classes.Generic" Data.Functor.Classes.Generic.Spec.spec
    describe "Data.Mergeable" Data.Mergeable.Spec.spec
    describe "Data.Source" Data.Source.Spec.spec
    describe "Data.Term" Data.Term.Spec.spec
    describe "Diffing.Algorithm.RWS" Diffing.Algorithm.RWS.Spec.spec
    describe "Diffing.Algorithm.SES" Diffing.Algorithm.SES.Spec.spec
    describe "Diffing.Interpreter" Diffing.Interpreter.Spec.spec
    describe "Rendering.TOC" Rendering.TOC.Spec.spec
    describe "Rendering.Imports" Rendering.Imports.Spec.spec
    describe "Semantic" Semantic.Spec.spec
    describe "Semantic.CLI" Semantic.CLI.Spec.spec
    describe "Semantic.IO" Semantic.IO.Spec.spec
    describe "Integration" Integration.Spec.spec
