{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Main (main) where

import           AST.Test
import           AST.Unmarshal
import qualified Language.Java.AST as Java
import           Language.Java.Grammar
import qualified System.Path as Path
import           Test.Tasty

main :: IO ()
main
  =   readCorpusFiles (Path.relDir "tree-sitter-java/vendor/tree-sitter-java/corpus")
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where
    parse = parseByteString @Java.Program @() tree_sitter_java

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-java corpus tests"
