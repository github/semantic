{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where


import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.TSX.AST as Tsx
import qualified System.Path.Fixture as Fixture
import           Test.Tasty
import           TreeSitter.TSX

main :: IO ()
main = do
#if BAZEL_BUILD
  rf <- Fixture.create
  let ?project = "external/semantic-typescript"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "tsx/corpus"
#else
  dirs <- Tsx.getTestCorpusDir
#endif

  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where parse = parseByteString @Tsx.Program @() tree_sitter_tsx

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-tsx corpus tests"
