{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Main (main) where

import qualified System.Path as Path
import           Test.Tasty
import           TreeSitter.Java
import qualified TreeSitter.Java.AST as Java
import           TreeSitter.Test.Helpers
import           TreeSitter.Unmarshal

main :: IO ()
main
  =   readCorpusFiles (Path.relDir "tree-sitter-java/vendor/tree-sitter-java/corpus")
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where
    parse = parseByteString @Java.Program @() tree_sitter_java

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-java corpus tests"
