{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main (main, graphFile) where

import           Analysis.Name (Name)
import qualified Analysis.Name as Name
import qualified AST.Unmarshal as TS
import           Control.Algebra
import           Control.Carrier.Lift
import           Control.Carrier.Sketch.ScopeGraph
import           Control.Effect.ScopeGraph
import qualified Control.Effect.ScopeGraph.Properties.Declaration as Props
import qualified Control.Effect.ScopeGraph.Properties.Function as Props
import qualified Control.Effect.ScopeGraph.Properties.Reference as Props
import           Control.Monad
import qualified Data.ByteString as ByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.ScopeGraph as ScopeGraph
import           Data.Semilattice.Lower
import qualified Language.Python ()
import qualified Language.Python as Py (Term)
import qualified Language.Python.Grammar as TSP
import           ScopeGraph.Algebraic
import           ScopeGraph.Convert
import qualified ScopeGraph.Properties.Declaration as Props
import qualified ScopeGraph.Properties.Function as Props
import qualified ScopeGraph.Properties.Reference as Props
import           Source.Loc
import qualified Source.Source as Source
import           Source.Span
import           System.Exit (die)
import           System.Path ((</>))
import qualified System.Path as Path
import qualified System.Path.Directory as Path
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

{-

The Python code here is

hello = ()
goodbye = ()

The graph should be

  🏁
  |
  1️⃣----"hello"
  |
  |
  |
  |
  2️⃣----"goodbye"

-}


runScopeGraph :: ToScopeGraph t => Path.AbsRelFile -> Source.Source -> t Loc -> (ScopeGraph.ScopeGraph Name, Result)
runScopeGraph p _src item = run . runSketch (Just p) $ scopeGraph item

sampleGraphThing :: (Has ScopeGraph sig m) => m Result
sampleGraphThing = do
  declare "hello" (Props.Declaration ScopeGraph.Assignment ScopeGraph.Default Nothing (Span (Pos 2 0) (Pos 2 10)))
  declare "goodbye" (Props.Declaration ScopeGraph.Assignment ScopeGraph.Default Nothing (Span (Pos 3 0) (Pos 3 12)))
  pure Complete

assertSimpleAssignment :: HUnit.Assertion
assertSimpleAssignment = do
  let path = "semantic-python/test/fixtures/1-04-toplevel-assignment.py"
  (result, Complete) <- graphFile path
  (expecto, Complete) <- runM $ runSketch Nothing sampleGraphThing
  HUnit.assertEqual "Should work for simple case" expecto result

expectedReference :: (Has ScopeGraph sig m) => m Result
expectedReference = do
  declare "x" (Props.Declaration ScopeGraph.Assignment ScopeGraph.Default Nothing (Span (Pos 0 0) (Pos 0 5)))
  reference "x" "x" Props.Reference
  pure Complete

assertSimpleReference :: HUnit.Assertion
assertSimpleReference = do
  let path = "semantic-python/test/fixtures/5-01-simple-reference.py"
  (result, Complete) <- graphFile path
  (expecto, Complete) <- runM $ runSketch Nothing expectedReference

  HUnit.assertEqual "Should work for simple case" expecto result

expectedLexicalScope :: (Has ScopeGraph sig m) => m Result
expectedLexicalScope = do
  _ <- declareFunction (Just $ Name.name "foo") (Props.Function ScopeGraph.Function (Span (Pos 0 0) (Pos 1 24)))
  reference "foo" "foo" Props.Reference {}
  pure Complete

expectedFunctionArg :: (Has ScopeGraph sig m) => m Result
expectedFunctionArg = do
  (_, associatedScope) <- declareFunction (Just $ Name.name "foo") (Props.Function ScopeGraph.Function (Span (Pos 0 0) (Pos 1 12)))
  withScope associatedScope $ do
    declare "x" (Props.Declaration ScopeGraph.Identifier ScopeGraph.Default Nothing lowerBound)
    reference "x" "x" Props.Reference
    pure ()
  reference "foo" "foo" Props.Reference
  pure Complete

expectedImportHole :: (Has ScopeGraph sig m) => m Result
expectedImportHole = do
  insertEdge ScopeGraph.Import (NonEmpty.fromList ["cheese", "ints"])
  pure Complete

assertLexicalScope :: HUnit.Assertion
assertLexicalScope = do
  let path = "semantic-python/test/fixtures/5-02-simple-function.py"
  (graph, _) <- graphFile path
  case run (runSketch Nothing expectedLexicalScope) of
    (expecto, Complete) -> HUnit.assertEqual "Should work for simple case" expecto graph
    (_, Todo msg)       -> HUnit.assertFailure ("Failed to complete:" <> show msg)

assertFunctionArg :: HUnit.Assertion
assertFunctionArg = do
  let path = "semantic-python/test/fixtures/5-03-function-argument.py"
  (graph, _) <- graphFile path
  case run (runSketch Nothing expectedFunctionArg) of
    (expecto, Complete) -> HUnit.assertEqual "Should work for simple case" expecto graph
    (_, Todo msg)       -> HUnit.assertFailure ("Failed to complete:" <>  show msg)

assertImportHole :: HUnit.Assertion
assertImportHole = do
  let path = "semantic-python/test/fixtures/cheese/6-01-imports.py"
  (graph, _) <- graphFile path
  case run (runSketch Nothing expectedImportHole) of
    (expecto, Complete) -> HUnit.assertEqual "Should work for simple case" expecto graph
    (_, Todo msg)       -> HUnit.assertFailure ("Failed to complete:" <>  show msg)

main :: IO ()
main = do
  -- make sure we're in the root directory so the paths resolve properly
  cwd <- Path.getCurrentDirectory
  when (Path.takeDirName cwd == Just (Path.relDir "semantic-python"))
    (Path.setCurrentDirectory (cwd </> Path.relDir ".."))

  Tasty.defaultMain $
    Tasty.testGroup "Tests" [
      Tasty.testGroup "declare" [
        HUnit.testCase "toplevel assignment" assertSimpleAssignment
      ],
      Tasty.testGroup "reference" [
        HUnit.testCase "simple reference" assertSimpleReference
      ],
      Tasty.testGroup "lexical scopes" [
        HUnit.testCase "simple function scope" assertLexicalScope
      , HUnit.testCase "simple function argument" assertFunctionArg
      ],
      Tasty.testGroup "imports" [
        HUnit.testCase "simple function argument" assertImportHole
      ]
    ]
