{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import           AST.Test
import           AST.Unmarshal
import qualified Language.JSON.AST as JSON
import           Language.JSON.Grammar
import qualified System.Path as Path
import           Test.Tasty

main :: IO ()
main
  =   Path.absDir <$> JSON.getTestCorpusDir
  >>= readCorpusFiles'
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @JSON.Document @() tree_sitter_json

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-json corpus tests"
