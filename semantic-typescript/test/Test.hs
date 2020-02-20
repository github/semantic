{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import           AST.Test
import           AST.Unmarshal
import qualified Language.TypeScript.AST as Ts
import           Language.TypeScript.Grammar
import qualified System.Path as Path
import           Test.Tasty

main :: IO ()
main
  =   readCorpusFiles (Path.relDir "tree-sitter-typescript/vendor/tree-sitter-typescript/typescript/corpus")
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @Ts.Program @() tree_sitter_typescript

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-typescript corpus tests"
