{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import qualified System.Path as Path
import           Test.Tasty
import           TreeSitter.Ruby
import qualified TreeSitter.Ruby.AST as Rb
import           TreeSitter.Test.Helpers
import           TreeSitter.Unmarshal

main :: IO ()
main
  =   readCorpusFiles (Path.relDir "tree-sitter-ruby/vendor/tree-sitter-ruby/test/corpus")
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @Rb.Program @() tree_sitter_ruby

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-ruby corpus tests"
