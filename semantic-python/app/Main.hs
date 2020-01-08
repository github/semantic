{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Main (main) where

import qualified Data.ByteString as ByteString
import Language.Python.ScopeGraph
import qualified TreeSitter.Python.AST as Py
import qualified TreeSitter.Python as TSP
import qualified TreeSitter.Unmarshal as TS
import Source.Span
import Source.Range

main :: IO ()
main = do
  file <- ByteString.readFile "semantic-python/test/fixtures/1-01-empty-module.py"
  tree <- TS.parseByteString @Py.Module @(Range, Span) TSP.tree_sitter_python file
  print (show tree)
