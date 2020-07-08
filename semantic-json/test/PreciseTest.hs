{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.JSON.AST as JSON
import           Language.JSON.Grammar
import qualified System.Path as Path
import           Test.Tasty

main :: IO ()
main = do
#if BAZEL_BUILD
  rf <- Fixture.create
  let ?project = Path.relDir "external/semantic-json"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "corpus"
#else
  dirs <- Path.absRel <$> JSON.getTestCorpusDir
#endif
  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where parse = parseByteString @(JSON.Document) @() tree_sitter_json

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-json corpus tests"
