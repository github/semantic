{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main (main) where

import           Control.Algebra
import           Control.Carrier.Sketch.Fresh
import           Convert.ToScopeGraph
import qualified Data.ByteString as ByteString
import           Data.Name (Name)
import qualified Data.ScopeGraph as ScopeGraph
import qualified Language.Python ()
import           Source.Loc
import qualified Source.Source as Source
import           System.Exit (die)
import qualified System.Path as Path
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

assertSimpleAssignment :: HUnit.Assertion
assertSimpleAssignment = do
  let path = "semantic-python/test/fixtures/1-04-toplevel-assignment.py"
  file <- ByteString.readFile path
  tree <- TS.parseByteString @Py.Module @Loc TSP.tree_sitter_python file
  pyModule <- either die pure tree
  let (expecto, Complete) = run $ runSketch Nothing sampleGraphThing
  let (result, Complete) = runScopeGraph (Path.absRel path) (Source.fromUTF8 file) pyModule
  HUnit.assertEqual "Should work for simple case" expecto result

main :: IO ()
main = Tasty.defaultMain (HUnit.testCase "toplevel assignment" assertSimpleAssignment)
