{-# LANGUAGE CPP, DisambiguateRecordFields, OverloadedStrings, TypeApplications, ImplicitParams #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where


import qualified Language.Go.AST as Go
import           Language.Go.Grammar
import           AST.TestHelpers
import           AST.Unmarshal
import           Test.Tasty
import qualified System.Path.Fixture as Fixture

main :: IO ()
main = do
#if BAZEL_BUILD
  rf <- Fixture.create
  let ?project = "external/tree-sitter-go"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "corpus"
#else
  dirs <- Go.getTestCorpusDir
#endif

  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where parse = parseByteString @Go.SourceFile @() tree_sitter_go

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-go corpus tests"
