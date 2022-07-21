{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main
  ( main,
  )
where

import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.Java.AST as Java
import qualified System.Path.Fixture as Fixture
import           Test.Tasty
import           TreeSitter.Java

main :: IO ()
main = do
#if BAZEL_BUILD
  rf <- Fixture.create
  --
  let ?project = "external/tree-sitter-java"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "corpus"
#else
  dirs <- Java.getTestCorpusDir
#endif
  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where
    parse = parseByteString @Java.Program @() tree_sitter_java

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-java corpus tests"
