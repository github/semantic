{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications, ImplicitParams #-}
module Main (main) where

import qualified System.Path as Path
import           Test.Tasty
import           TreeSitter.Python
import qualified Language.Python.AST as Py
import           AST.TestHelpers
import           AST.Unmarshal
import qualified Bazel.Runfiles as Runfiles
import qualified System.Path.Fixture as Fixture

main :: IO ()
main = do
  rf <- Runfiles.create
  -- dirs <- Path.absDir <$> Ruby.getTestCorpusDir
  let ?project = Path.relDir "semantic-python"
      ?runfiles = rf

  let dirs = Fixture.bazelDir "/../external/tree-sitter-python/test/corpus"
      parse = parseByteString @Py.Module @() tree_sitter_python

  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-python corpus tests"
