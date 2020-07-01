{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications, ImplicitParams #-}
module Main (main) where

import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.CodeQL.AST as CodeQL
import           Language.CodeQL.Grammar
import qualified System.Path as Path
import           Test.Tasty
import qualified System.Path.Fixture as Fixture
import qualified Bazel.Runfiles as Runfiles

main :: IO ()
main = do
  rf <- Runfiles.create
  -- dirs <- Path.absDir <$> Ruby.getTestCorpusDir
  let ?project = Path.relDir "external/tree-sitter-ql"
      ?runfiles = rf

  let dirs = Fixture.absRelDir "test/corpus"
      parse = parseByteString @CodeQL.Ql @() tree_sitter_ql

  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-ql corpus tests"
