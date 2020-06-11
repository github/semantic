{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.CodeQL.AST as CodeQL
import           Language.CodeQL.Grammar
import qualified System.Path as Path
import           Test.Tasty

main :: IO ()
main
  =   Path.absDir <$> CodeQL.getTestCorpusDir
  >>= readCorpusFiles'
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @CodeQL.Ql @() tree_sitter_ql

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-ql corpus tests"
