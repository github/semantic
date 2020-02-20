{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import           AST.Test
import           AST.Unmarshal
import qualified Language.TSX.AST as Tsx
import           Language.TSX.Grammar
import qualified System.Path as Path
import           Test.Tasty

main :: IO ()
main
  =   readCorpusFiles (Path.relDir "tree-sitter-tsx/vendor/tree-sitter-typescript/tsx/corpus")
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @Tsx.Program @() tree_sitter_tsx

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-tsx corpus tests"
