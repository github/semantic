{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import qualified System.Path as Path
import           Test.Tasty
import           TreeSitter.Python
import qualified TreeSitter.Python.AST as Py
import           TreeSitter.Test.Helpers
import           TreeSitter.Unmarshal

main :: IO ()
main
  =   readCorpusFiles (Path.relDir "tree-sitter-python/vendor/tree-sitter-python/test/corpus")
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @Py.Module @() tree_sitter_python

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-python corpus tests"
