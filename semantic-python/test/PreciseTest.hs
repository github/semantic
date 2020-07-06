{-# LANGUAGE CPP, DisambiguateRecordFields, OverloadedStrings, TypeApplications, ImplicitParams #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where

import qualified System.Path as Path
import           Test.Tasty
import           TreeSitter.Python
import qualified Language.Python.AST as Py
import           AST.TestHelpers
import           AST.Unmarshal
import qualified System.Path.Fixture as Fixture

main :: IO ()
main = do
#if BAZEL_BUILD
  rf <- Fixture.create
  let ?project = Path.relDir "external/tree-sitter-python"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "test/corpus"
#else
  dirs <- Path.absRel <$> Py.getTestCorpusDir
#endif

  let parse = parseByteString @Py.Module @() tree_sitter_python

  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-python corpus tests"
