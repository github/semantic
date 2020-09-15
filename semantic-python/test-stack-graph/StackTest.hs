{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import qualified AST.Unmarshal as TS
import Analysis.Name (Name)
import qualified Analysis.Name as Name
import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.StackGraph
import qualified Control.Effect.StackGraph.Properties.Declaration as Props
import qualified Control.Effect.StackGraph.Properties.Function as Props
import qualified Control.Effect.StackGraph.Properties.Reference as Props
import Control.Monad
import qualified Data.ByteString as ByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.ScopeGraph as ScopeGraph
import Data.Semilattice.Lower
import Debug.Trace
import qualified Language.Python ()
import qualified Language.Python as Py (Term)
import qualified Language.Python.Grammar as TSP
import Scope.Graph.Convert
import Source.Loc
import qualified Source.Source as Source
import Source.Span
import qualified Stack.Graph as Stack
import System.Exit (die)
import System.Path ((</>))
import qualified System.Path as Path
import qualified System.Path.Directory as Path
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

runStackGraph :: ToScopeGraph t => Path.AbsRelFile -> Source.Source -> t Loc -> (Stack.Graph Stack.Node, Result)
runStackGraph p _src item = (\(stack, (scopeGraph, result)) -> (stack, result)) . run . runStackGraph minfo $ scopeGraph item
  where
    minfo = lowerBound

runStackGraphTest :: Monad m => StackGraphC Name m Result -> m (Stack.Graph Stack.Node, Result)
runStackGraphTest val = do
  result <- runStackGraph lowerBound $ val
  pure ((\(stack, (scopeGraph, result)) -> (stack, result)) result)

stackGraphFile :: FilePath -> IO (Stack.Graph Stack.Node, Result)
stackGraphFile fp = do
  file <- ByteString.readFile fp
  tree <- TS.parseByteString @Py.Term @Loc TSP.tree_sitter_python file
  pyModule <- either die pure tree
  pure $ runStackGraph (Path.absRel fp) (Source.fromUTF8 file) pyModule

expectedQualifiedImport :: StackGraphEff sig m => m Result
expectedQualifiedImport = do
  ScopeGraph.CurrentScope currentName <- currentScope
  name <- newScope (Map.singleton ScopeGraph.Lexical [currentName])
  putCurrentScope name
  pure Complete

assertQualifiedImport :: HUnit.Assertion
assertQualifiedImport = do
  let path = "semantic-python/test/fixtures/cheese/6-02-qualified-imports.py"
  (graph, _) <- stackGraphFile path
  (expecto, Complete) <- runStackGraphTest expectedQualifiedImport
  traceShowM expecto
  HUnit.assertEqual "Should work for simple case" expecto graph

main :: IO ()
main = do
  -- make sure we're in the root directory so the paths resolve properly
  cwd <- Path.getCurrentDirectory
  when
    (Path.takeDirName cwd == Just (Path.relDir "semantic-python"))
    (Path.setCurrentDirectory (cwd </> Path.relDir ".."))

  Tasty.defaultMain $
    Tasty.testGroup
      "stack graph"
      [ HUnit.testCase "qualified import" assertQualifiedImport
      ]
