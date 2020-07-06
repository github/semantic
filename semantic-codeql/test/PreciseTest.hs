{-# LANGUAGE CPP, DisambiguateRecordFields, OverloadedStrings, TypeApplications, ImplicitParams #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where

import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.CodeQL.AST as CodeQL
import           Language.CodeQL.Grammar
import qualified System.Path as Path
import           Test.Tasty
import qualified System.Path.Fixture as Fixture

main :: IO ()
main = do
#if BAZEL_BUILD
  rf <- Fixture.create
  let ?project = Path.relDir "external/tree-sitter-ql"
      ?runfiles = rf

  let dirs = Fixture.absRelDir "test/corpus"
#else
  dirs <- Path.absRel <$> CodeQL.getTestCorpusDir
#endif
  let parse = parseByteString @CodeQL.Ql @() tree_sitter_ql

  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-ql corpus tests"
