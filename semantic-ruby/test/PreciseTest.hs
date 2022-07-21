{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where

import           AST.TestHelpers
import           AST.Unmarshal
import           Control.Concurrent
import qualified Language.Ruby.AST as Ruby
import           System.IO
import qualified System.Path.Fixture as Fixture
import           Test.Tasty
import           TreeSitter.Ruby

main :: IO ()
main = do
#if BAZEL_BUILD
  rf <- Fixture.create
  let ?project = "external/tree-sitter-ruby"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "test/corpus"
#else
  dirs <- Ruby.getTestCorpusDir
#endif
  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where parse = parseByteString @Ruby.Program @() tree_sitter_ruby

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-ruby corpus tests"
