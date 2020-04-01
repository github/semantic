{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import qualified System.Path as Path
import           Test.Tasty
import           TreeSitter.TypeScript
import qualified TreeSitter.TypeScript.AST as Ts
import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.TypeScript.AST as Ts
import           Language.TypeScript.Grammar
import qualified System.Path as Path
import           Test.Tasty

main :: IO ()
main
  =   Path.absDir <$> Ts.getTestCorpusDir
  >>= readCorpusFiles'
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @Ts.Program @() tree_sitter_typescript

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-typescript corpus tests"
