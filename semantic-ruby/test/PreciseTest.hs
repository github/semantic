{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications, ImplicitParams #-}
module Main (main) where

import           TreeSitter.Ruby
import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.Ruby.AST as Ruby
import qualified System.Path as Path
import           Test.Tasty
import qualified Bazel.Runfiles as Runfiles
import qualified System.Path.Fixture as Fixture
import System.IO
import Control.Concurrent

main :: IO ()
main = do
  rf <- Runfiles.create
  -- dirs <- Path.absDir <$> Ruby.getTestCorpusDir
  let ?project = Path.relDir "semantic-ruby"
      ?runfiles = rf
  let dirs = Fixture.bazelDir "/../external/tree-sitter-ruby/test/corpus"
  hFlush stdout
  threadDelay 0

  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where parse = parseByteString @Ruby.Program @() tree_sitter_ruby

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-ruby corpus tests"
