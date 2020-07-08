{-# LANGUAGE CPP, DisambiguateRecordFields, OverloadedStrings, TypeApplications, ImplicitParams #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where

import           TreeSitter.Ruby
import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.Ruby.AST as Ruby
import qualified System.Path as Path
import           Test.Tasty
import qualified System.Path.Fixture as Fixture
import System.IO
import Control.Concurrent

main :: IO ()
main = do
#if BAZEL_BUILD
  rf <- Fixture.create
  let ?project = Path.relDir "external/tree-sitter-ruby"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "test/corpus"
#else
  dirs <- Path.absRel <$> Ruby.getTestCorpusDir
#endif
  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where parse = parseByteString @Ruby.Program @() tree_sitter_ruby

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-ruby corpus tests"
