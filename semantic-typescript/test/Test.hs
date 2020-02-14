{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import qualified System.Path as Path
import           Test.Tasty
import           TreeSitter.TypeScript
import qualified TreeSitter.TypeScript.AST as Ts
import           AST.TestHelpers
import           AST.Unmarshal

main :: IO ()
main
  =   readCorpusFiles (Path.relDir "tree-sitter-typescript/vendor/tree-sitter-typescript/typescript/corpus")
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @Ts.Program @() tree_sitter_typescript

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-typescript corpus tests"
