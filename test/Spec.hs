{-# LANGUAGE ImplicitParams #-}

module Main (allTests, legacySpecs, main, tests) where

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
import qualified Data.Language.Spec
import qualified Data.Scientific.Spec
import qualified Data.Semigroup.App.Spec
import qualified Data.Term.Spec
import qualified Diffing.Algorithm.RWS.Spec
import qualified Diffing.Algorithm.SES.Spec
import qualified Diffing.Interpreter.Spec
import qualified Graphing.Calls.Spec
import qualified Integration.Spec
import qualified Numeric.Spec
import qualified Parsing.Spec
import qualified Rendering.TOC.Spec
import qualified Tags.Spec
import qualified Semantic.Spec
import qualified Semantic.CLI.Spec
import qualified Semantic.IO.Spec
import qualified Semantic.Stat.Spec
import Semantic.Config (defaultOptions, optionsLogLevel)
import Semantic.Task (withOptions, TaskSession(..))
import Test.Hspec
import Test.Tasty as Tasty
import Test.Tasty.Hspec as Tasty

tests :: (?session :: TaskSession) => [TestTree]
tests =
  [ Data.Language.Spec.testTree
  , Data.Scientific.Spec.testTree
  , Data.Semigroup.App.Spec.testTree
  , Integration.Spec.testTree
  , Numeric.Spec.testTree
  , Semantic.CLI.Spec.testTree
  , Semantic.Stat.Spec.testTree
  ]

-- We can't bring this out of the IO monad until we divest
-- from hspec, since testSpec operates in IO.
allTests :: (?session :: TaskSession) => IO TestTree
allTests = do
  asTastySpecs <- Tasty.testSpecs legacySpecs
  let allSpecs = tests <> asTastySpecs
  pure . Tasty.localOption Tasty.Success $ testGroup "semantic" allSpecs

-- If you're writing new test modules, please don't add to this
-- stanza: it is only there to prevent massive rewrites, and is
-- converted into a Tasty TestTree in 'main'. (Quoth the tasty-hspec
-- documentation: "hspec and tasty serve similar purposes; consider
-- using one or the other.") Instead, create a new TestTree value
-- in your spec module and add it to the above 'tests' list.
legacySpecs :: (?session :: TaskSession) => Spec
legacySpecs = parallel $ do
  describe "Analysis.Go" Analysis.Go.Spec.spec
  describe "Analysis.PHP" Analysis.PHP.Spec.spec
  describe "Analysis.Python" Analysis.Python.Spec.spec
  describe "Analysis.Ruby" Analysis.Ruby.Spec.spec
  describe "Analysis.TypeScript" Analysis.TypeScript.Spec.spec
  describe "Assigning.Assignment" Assigning.Assignment.Spec.spec
  describe "Control.Abstract.Evaluator" Control.Abstract.Evaluator.Spec.spec
  describe "Data.Diff" Data.Diff.Spec.spec
  describe "Data.Graph" Data.Graph.Spec.spec
  describe "Data.Abstract.Path" Data.Abstract.Path.Spec.spec
  describe "Data.Abstract.Name" Data.Abstract.Name.Spec.spec
  describe "Data.Functor.Classes.Generic" Data.Functor.Classes.Generic.Spec.spec
  describe "Data.Term" Data.Term.Spec.spec
  describe "Diffing.Algorithm.RWS" Diffing.Algorithm.RWS.Spec.spec
  describe "Diffing.Algorithm.SES" Diffing.Algorithm.SES.Spec.spec
  describe "Diffing.Interpreter" Diffing.Interpreter.Spec.spec
  describe "Graphing.Calls" Graphing.Calls.Spec.spec
  describe "Rendering.TOC" Rendering.TOC.Spec.spec
  describe "Tags.Spec" Tags.Spec.spec
  describe "Semantic" Semantic.Spec.spec
  describe "Semantic.IO" Semantic.IO.Spec.spec
  describe "Parsing" Parsing.Spec.spec


main :: IO ()
main = do
  withOptions defaultOptions { optionsLogLevel = Nothing } $ \ config logger statter ->
    let ?session = TaskSession config "-" False logger statter
    in allTests >>= defaultMain
