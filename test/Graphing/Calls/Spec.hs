{-# LANGUAGE PackageImports #-}

module Graphing.Calls.Spec ( spec ) where

import Prelude hiding (readFile)
import Prologue
import SpecHelpers hiding (readFile)

import Algebra.Graph
import Data.List (uncons)

import           "semantic" Data.Graph (Graph (..), topologicalSort)
import           Data.Graph.Vertex
import qualified Data.Language as Language
import           Semantic.Config (defaultOptions)
import           Semantic.Graph
import           Semantic.IO

callGraphPythonProject paths = runTaskWithOptions defaultOptions $ do
  let proxy = Proxy @'Language.Python
  let lang = Language.Python
  blobs <- catMaybes <$> traverse readFile (flip File lang <$> paths)
  package <- parsePackage pythonParser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs lang [])
  modules <- topologicalSort <$> runImportGraph proxy package
  runCallGraph proxy False modules package

spec :: Spec
spec = describe "call graphing" $ do

  it "should work for a simple example" $ do
    res <- callGraphPythonProject ["test/fixtures/python/graphing/simple/simple.py"]
    unGraph res `shouldSatisfy` hasVertex (Variable "magnus")

  it "should evaluate both sides of an if-statement" $ do
    res <- callGraphPythonProject ["test/fixtures/python/graphing/conditional/conditional.py"]
    unGraph res `shouldSatisfy` hasVertex (Variable "merle")
    unGraph res `shouldSatisfy` hasVertex (Variable "taako")
