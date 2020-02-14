{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import qualified System.Path as Path
import           Test.Tasty
import           Language.Go.Grammar
import qualified Language.Go.AST as Go
import           AST.Test
import           AST.Unmarshal

main :: IO ()
main
  =   readCorpusFiles (Path.relDir "tree-sitter-go/vendor/tree-sitter-go/corpus")
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @Go.SourceFile @() tree_sitter_go

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-go corpus tests"
