{-# LANGUAGE CPP, DisambiguateRecordFields, OverloadedStrings, TypeApplications, ImplicitParams #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where

import           TreeSitter.TypeScript
import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.TypeScript.AST as Ts
import qualified System.Path as Path
import           Test.Tasty
import qualified System.Path.Fixture as Fixture

main :: IO ()
main = do
#if BAZEL_BUILD
  rf <- Fixture.create
  let ?project = Path.relDir "external/tree-sitter-typescript"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "typescript/corpus"
#else
  dirs <- Path.absRel <$> Ts.getTestCorpusDir
#endif

  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where parse = parseByteString @Ts.Program @() tree_sitter_typescript

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-typescript corpus tests"
