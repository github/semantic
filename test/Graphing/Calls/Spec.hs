{-# LANGUAGE DataKinds, GADTs, OverloadedStrings, PackageImports, TypeApplications #-}

module Graphing.Calls.Spec ( spec ) where

import Prelude hiding (readFile)
import SpecHelpers

import Algebra.Graph

import           Control.Effect.Parse
import           "semantic" Data.Graph (Graph (..), topologicalSort)
import           Data.Graph.ControlFlowVertex
import qualified Data.Language as Language
import           Data.Maybe (fromJust)
import           Semantic.Graph
import qualified System.Path as Path

callGraphPythonProject :: Path.RelFile -> IO (Semantic.Graph.Graph ControlFlowVertex)
callGraphPythonProject path = runTaskOrDie $ do
  let proxy = Proxy @'Language.Python
      lang = Language.Python
  SomeParser parser <- pure . fromJust $! parserForLanguage analysisParsers Language.Python
  blob <- readBlobFromFile' (fileForTypedPath path)
  package <- fmap snd <$> parsePackage parser (Project (Path.toString (Path.takeDirectory path)) [blob] lang [])
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  runCallGraph proxy False modules package

spec :: Spec
spec = describe "call graphing" $ do

  let needs r v = unGraph r `shouldSatisfy` hasVertex v

  it "should work for a simple example" $ do
    res <- callGraphPythonProject (Path.relFile "test/fixtures/python/graphing/simple/simple.py")
    res `needs` Variable "magnus" "simple.py" (Span (Pos 4 1) (Pos 4 7))

  it "should evaluate both sides of an if-statement" $ do
    res <- callGraphPythonProject (Path.relFile "test/fixtures/python/graphing/conditional/conditional.py")
    res `needs` Variable "merle" "conditional.py" (Span (Pos 5 5) (Pos 5 10))
    res `needs` Variable "taako" "conditional.py" (Span (Pos 8 5) (Pos 8 10))

  it "should continue even when a type error is encountered" $ do
    res <- callGraphPythonProject (Path.relFile "test/fixtures/python/graphing/typeerror/typeerror.py")
    res `needs` Variable "lup" "typeerror.py" (Span (Pos 5 1) (Pos 5 4))

  it "should continue when an unbound variable is encountered" $ do
    res <- callGraphPythonProject (Path.relFile "test/fixtures/python/graphing/unbound/unbound.py")
    res `needs` Variable "lucretia" "unbound.py" (Span (Pos 5 1) (Pos 5 9))
