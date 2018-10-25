{-# LANGUAGE PackageImports #-}

module Graphing.Calls.Spec ( spec ) where

import Prelude hiding (readFile)
import Prologue
import SpecHelpers hiding (readFile)

import Algebra.Graph
import Data.List (uncons)

import           Data.Abstract.Module
import           "semantic" Data.Graph (Graph (..), topologicalSort)
import           Data.Graph.ControlFlowVertex
import           Data.Span
import qualified Data.Language as Language
import           Semantic.Config (defaultOptions)
import           Semantic.Graph
import           Semantic.IO

callGraphPythonProject paths = runTaskWithOptions defaultOptions $ do
  let proxy = Proxy @'Language.Python
  let lang = Language.Python
  blobs <- catMaybes <$> traverse readBlobFromFile (flip File lang <$> paths)
  package <- fmap snd <$> parsePackage pythonParser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs lang [])
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  runCallGraph proxy False modules package

spec :: Spec
spec = describe "call graphing" $ do

  let needs r v = unGraph r `shouldSatisfy` hasVertex v

  it "should work for a simple example" $ do
    res <- callGraphPythonProject ["test/fixtures/python/graphing/simple/simple.py"]
    res `needs` Variable "magnus" "simple.py" (Span (Pos 4 1) (Pos 4 7))

  it "should evaluate both sides of an if-statement" $ do
    res <- callGraphPythonProject ["test/fixtures/python/graphing/conditional/conditional.py"]
    res `needs` Variable "merle" "conditional.py" (Span (Pos 5 5) (Pos 5 10))
    res `needs` Variable "taako" "conditional.py" (Span (Pos 8 5) (Pos 8 10))

  it "should continue even when a type error is encountered" $ do
    res <- callGraphPythonProject ["test/fixtures/python/graphing/typeerror/typeerror.py"]
    res `needs` Variable "lup" "typeerror.py" (Span (Pos 5 1) (Pos 5 4))

  it "should continue when an unbound variable is encountered" $ do
    res <- callGraphPythonProject ["test/fixtures/python/graphing/unbound/unbound.py"]
    res `needs` Variable "lucretia" "unbound.py" (Span (Pos 5 1) (Pos 5 9))
