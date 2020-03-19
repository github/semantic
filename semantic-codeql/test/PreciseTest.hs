{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import qualified System.Path as Path
import           Test.Tasty
import qualified Language.CodeQL.AST as CodeQL
import           Language.CodeQL.Grammar
import           AST.Test
import           AST.Unmarshal

main :: IO ()
main
  =   Path.absDir <$> CodeQL.getTestCorpusDir
  >>= readCorpusFiles'
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @CodeQL.CodeQl @() tree_sitter_ql

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-ql corpus tests"
