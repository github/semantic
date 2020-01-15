{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main (main) where

import           Control.Algebra
import           Control.Carrier.Sketch.Fresh
import           Control.Monad
import           Convert.ToScopeGraph
import qualified Data.ByteString as ByteString
import           Data.Name (Name)
import qualified Data.ScopeGraph as ScopeGraph
import qualified Language.Python ()
import           Source.Loc
import qualified Source.Source as Source
import           System.Exit (die)
import           System.Path ((</>))
import qualified System.Path as Path
import qualified System.Path.Directory as Path
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified TreeSitter.Python as TSP
import qualified TreeSitter.Python.AST as Py
import qualified TreeSitter.Unmarshal as TS

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

sampleGraphThing :: (Has (Sketch Name) sig m) => m Result
sampleGraphThing = do
  declare @Name "hello" DeclProperties
  declare @Name "goodbye" DeclProperties
  pure Complete

graphFile :: FilePath -> IO (ScopeGraph.ScopeGraph Name, Result)
graphFile fp = do
  file <- ByteString.readFile fp
  tree <- TS.parseByteString @Py.Module @Loc TSP.tree_sitter_python file
  pyModule <- either die pure tree
  pure $ runScopeGraph (Path.absRel fp) (Source.fromUTF8 file) pyModule


assertSimpleAssignment :: HUnit.Assertion
assertSimpleAssignment = do
  let path = "semantic-python/test/fixtures/1-04-toplevel-assignment.py"
  (result, Complete) <- graphFile path
  let (expecto, Complete) = run $ runSketch Nothing sampleGraphThing
  HUnit.assertEqual "Should work for simple case" expecto result

expectedReference :: (Has (Sketch Name) sig m) => m Result
expectedReference = do
  declare @Name "x" DeclProperties
  reference @Name "x" "x" RefProperties
  pure Complete

assertSimpleReference :: HUnit.Assertion
assertSimpleReference = do
  let path = "semantic-python/test/fixtures/5-01-simple-reference.py"
  (result, Complete) <- graphFile path
  let (expecto, Complete) = run $ runSketch Nothing expectedReference

  HUnit.assertEqual "Should work for simple case" expecto result

expectedLexicalScope :: (Has (Sketch Name) sig m) => m Result
expectedLexicalScope = do
  declare @Name "x" DeclProperties
  reference @Name "x" "x" RefProperties
  pure Complete

assertLexicalScope :: HUnit.Assertion
assertLexicalScope = do
  let path = "semantic-python/test/fixtures/5-02-simple-function.py"
  (result, Complete) <- graphFile path
  let (expecto, Complete) = run $ runSketch Nothing expectedReference

  HUnit.assertEqual "Should work for simple case" expecto result

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
        HUnit.testCase "simple scope" assertLexicalScope
      ]
    ]
