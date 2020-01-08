{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Main (main) where

import qualified Data.ByteString as ByteString
import qualified Data.ScopeGraph as ScopeGraph
import qualified Data.ScopeGraph as ScopeGraph
import qualified Language.Python ()
import qualified TreeSitter.Python.AST as Py
import qualified TreeSitter.Python as TSP
import qualified TreeSitter.Unmarshal as TS
import qualified Source.Source as Source
import Source.Span
import Source.Range
import Source.Loc
import System.Exit (die)

main :: IO ()
main = do
  file <- ByteString.readFile "semantic-python/test/fixtures/1-01-empty-module.py"
  tree <- TS.parseByteString @Py.Module @Loc TSP.tree_sitter_python file
  pyModule <- either die pure tree
  print =<< (ScopeGraph.runScopeGraph (Source.fromUTF8 file) pyModule)
