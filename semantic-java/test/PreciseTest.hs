{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Main (main) where


import           TreeSitter.Java
import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.Java.AST as Java
import qualified System.Path as Path
import           Test.Tasty

main :: IO ()
main
  =   Path.absDir <$> Java.getTestCorpusDir
  >>= readCorpusFiles'
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where
    parse = parseByteString @Java.Program @() tree_sitter_java

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-java corpus tests"
