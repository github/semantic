{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications, ImplicitParams #-}
module Main (main) where


import qualified Language.Go.AST as Go
import           Language.Go.Grammar
import           AST.TestHelpers
import           AST.Unmarshal
import qualified System.Path as Path
import           Test.Tasty
import qualified Bazel.Runfiles as Runfiles
import qualified System.Path.Fixture as Fixture

main :: IO ()
main = do
  rf <- Runfiles.create
  -- dirs <- Path.absDir <$> Go.getTestCorpusDir
  let ?project = Path.relDir "external/tree-sitter-go"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "corpus"

  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where parse = parseByteString @Go.SourceFile @() tree_sitter_go

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-go corpus tests"
