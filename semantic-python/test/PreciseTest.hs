{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import qualified System.Path as Path
import           Test.Tasty
import qualified Language.Python.AST as Py
import           Language.Python.Grammar
import           AST.Test
import           AST.Unmarshal

main :: IO ()
main
  =   Path.absDir <$> Py.getTestCorpusDir
  >>= readCorpusFiles'
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @Py.Module @() tree_sitter_python

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-python corpus tests"
