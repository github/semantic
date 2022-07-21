{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where

import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.Python.AST as Py
import qualified System.Path.Fixture as Fixture
import           Test.Tasty
import           TreeSitter.Python

main :: IO ()
main = do
#if BAZEL_BUILD
  rf <- Fixture.create
  let ?project = "external/tree-sitter-python"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "test/corpus"
#else
  dirs <- Py.getTestCorpusDir
#endif

  let parse = parseByteString @Py.Module @() tree_sitter_python

  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-python corpus tests"
