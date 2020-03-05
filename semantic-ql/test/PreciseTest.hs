{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import qualified System.Path as Path
import           Test.Tasty
import qualified Language.QL.AST as QL
import           Language.QL.Grammar
import           AST.Test
import           AST.Unmarshal

main :: IO ()
main
  =   Path.absDir <$> QL.getTestCorpusDir
  >>= readCorpusFiles'
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @QL.Ql @() tree_sitter_ql

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-ql corpus tests"
