{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import AST.TestHelpers
import AST.Unmarshal
import qualified Bazel.Runfiles as Runfiles
import qualified Language.Java.AST as Java
import qualified System.Path as Path
import qualified System.Path.Fixture as Fixture
import Test.Tasty
import TreeSitter.Java

main :: IO ()
main = do
  rf <- Runfiles.create
  -- dirs <- Path.absDir <$> Java.getTestCorpusDir
  let ?project = Path.relDir "semantic-java"
      ?runfiles = rf
  let dirs = Fixture.bazelDir "/../external/tree-sitter-java/corpus"
  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where
    parse = parseByteString @Java.Program @() tree_sitter_java

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-java corpus tests"
