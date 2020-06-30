{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.Ruby.AST as Rb
import qualified System.Path as Path
import qualified System.Path.Fixture as Fixture
import qualified Bazel.Runfiles as Bazel
import           Test.Tasty
import System.IO
import Control.Concurrent
import           TreeSitter.Ruby

main :: IO ()
main = do
  let ?project = Path.relDir "semantic-ruby"

  rf <- Bazel.create
  let ?runfiles = rf

  let parse = parseByteString @Rb.Program @() tree_sitter_ruby

  Rb.getTestCorpusDir >>= print
  print (Path.toString (Fixture.bazelDir "/."))
  hFlush stdout
  threadDelay 1000000000

  Fixture.bazelDir <$> Rb.getTestCorpusDir
    >>= readCorpusFiles'
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-ruby corpus tests"
