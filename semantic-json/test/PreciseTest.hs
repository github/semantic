{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           AST.TestHelpers
import           AST.Unmarshal
import           Data.Functor.Identity
import qualified Language.JSON.AST as JSON
import           Language.JSON.Grammar
import qualified System.Path as Path
import qualified System.Path as Path
import           Test.Tasty
import           Test.Tasty
import           TreeSitter.JSON

main :: IO ()
main
  =   Path.absDir <$> JSON.getTestCorpusDir
  >>= readCorpusFiles'
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @(JSON.Document Identity) @() tree_sitter_json

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-json corpus tests"
