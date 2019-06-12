{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Analysis.Go.Spec
import qualified Analysis.PHP.Spec
import qualified Analysis.Python.Spec
import qualified Analysis.Ruby.Spec
import qualified Analysis.TypeScript.Spec
import qualified Assigning.Assignment.Spec
import qualified Control.Abstract.Evaluator.Spec
import qualified Data.Diff.Spec
import qualified Data.Abstract.Name.Spec
import qualified Data.Abstract.Path.Spec
import qualified Data.Functor.Classes.Generic.Spec
import qualified Data.Graph.Spec
import qualified Data.Range.Spec
import qualified Data.Scientific.Spec
import qualified Data.Semigroup.App.Spec
import qualified Data.Source.Spec
import qualified Data.Term.Spec
import qualified Diffing.Algorithm.RWS.Spec
import qualified Diffing.Algorithm.SES.Spec
import qualified Diffing.Interpreter.Spec
import qualified Graphing.Calls.Spec
import qualified Integration.Spec
import qualified Numeric.Spec
import qualified Parsing.Spec
import qualified Rendering.TOC.Spec
import qualified Reprinting.Spec
import qualified Rewriting.Go.Spec
import qualified Rewriting.JSON.Spec
import qualified Rewriting.Python.Spec
import qualified Tags.Spec
import qualified Semantic.Spec
import qualified Semantic.CLI.Spec
import qualified Semantic.IO.Spec
import qualified Semantic.Stat.Spec
import Semantic.Config (defaultOptions, optionsLogLevel)
import Semantic.Task (withOptions, TaskSession(..))
import Test.Tasty
import Test.Hspec
import Control.Exception

tastySpecs :: TaskSession -> [TestTree]
tastySpecs args
  = Integration.Spec.spec args
  : Semantic.Stat.Spec.spec

  -- : "Analysis.Go" (Analysis.Go.Spec.spec args)
  -- : "Analysis.PHP" (Analysis.PHP.Spec.spec args)
  -- : "Analysis.Python" (Analysis.Python.Spec.spec args)
  -- : "Analysis.Ruby" (Analysis.Ruby.Spec.spec args)
  -- : "Analysis.TypeScript" (Analysis.TypeScript.Spec.spec args)
  -- : "Assigning.Assignment" Assigning.Assignment.Spec.spec
  : Control.Abstract.Evaluator.Spec.spec
  : Data.Diff.Spec.spec
  : Data.Graph.Spec.spec
  -- : "Data.Abstract.Path" Data.Abstract.Path.Spec.spec
  : Data.Abstract.Name.Spec.spec
  : Data.Functor.Classes.Generic.Spec.spec
  : Data.Range.Spec.spec
  : Data.Scientific.Spec.spec
  : Data.Semigroup.App.Spec.spec
  -- : "Data.Source" Data.Source.Spec.spec
  : Data.Term.Spec.spec
  -- : "Diffing.Algorithm.RWS" Diffing.Algorithm.RWS.Spec.spec
  -- : "Diffing.Algorithm.SES" Diffing.Algorithm.SES.Spec.spec
  -- : "Diffing.Interpreter" Diffing.Interpreter.Spec.spec
  -- : "Graphing.Calls" Graphing.Calls.Spec.spec
  : Numeric.Spec.spec
  -- : "Rendering.TOC" Rendering.TOC.Spec.spec
  -- : "Reprinting.Spec" Reprinting.Spec.spec
  : Rewriting.Go.Spec.spec
  -- : "Rewriting.JSON" Rewriting.JSON.Spec.spec
  : Rewriting.Python.Spec.spec
  : Tags.Spec.spec
  -- : "Semantic" Semantic.Spec.spec
  : Semantic.CLI.Spec.spec
  : Semantic.IO.Spec.spec
  : Parsing.Spec.spec
  : []

hspecSpecs :: TaskSession -> Spec
hspecSpecs args =
  parallel $ do
    describe "Analysis.Go" (Analysis.Go.Spec.spec args)
    describe "Analysis.PHP" (Analysis.PHP.Spec.spec args)
    describe "Analysis.Python" (Analysis.Python.Spec.spec args)
    describe "Analysis.Ruby" (Analysis.Ruby.Spec.spec args)
    describe "Analysis.TypeScript" (Analysis.TypeScript.Spec.spec args)
    describe "Assigning.Assignment" Assigning.Assignment.Spec.spec
    describe "Data.Source" Data.Source.Spec.spec
    describe "Diffing.Algorithm.RWS" Diffing.Algorithm.RWS.Spec.spec
    describe "Diffing.Algorithm.SES" Diffing.Algorithm.SES.Spec.spec
    describe "Diffing.Interpreter" Diffing.Interpreter.Spec.spec
    describe "Graphing.Calls" Graphing.Calls.Spec.spec
    describe "Rendering.TOC" Rendering.TOC.Spec.spec
    describe "Reprinting.Spec" Reprinting.Spec.spec
    describe "Rewriting.JSON" Rewriting.JSON.Spec.spec
    describe "Semantic" Semantic.Spec.spec

main :: IO ()
main = do
  withOptions defaultOptions { optionsLogLevel = Nothing } $ \ config logger statter -> do
    let args = TaskSession config "-" False logger statter
    --hspec (hspecSpecs args) `catch` (\(e :: SomeException) -> pure ())
    defaultMain (testGroup "Semantic" (tastySpecs args))

