{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import qualified System.Path as Path
import           Test.Tasty
import           TreeSitter.JSON
import qualified TreeSitter.JSON.AST as JSON
import           TreeSitter.Test.Helpers
import           TreeSitter.Unmarshal

main :: IO ()
main
  =   readCorpusFiles (Path.relDir "tree-sitter-json/vendor/tree-sitter-json/corpus")
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @JSON.Document @() tree_sitter_json

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-json corpus tests"
