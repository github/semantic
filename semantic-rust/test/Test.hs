{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           AST.TestHelpers
import           AST.Unmarshal (parseByteString)
import           Control.Monad (liftM)
import qualified Language.Rust.AST as Rust
import           Language.Rust.Grammar
import           System.FilePath
import           Test.Tasty

main :: IO ()
main = do
#if BAZEL_BUILD
  rf <- Fixture.create
  let ?project = "external/tree-sitter-python"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "test/corpus"
#else
  dirs <- Rust.getTestCorpusDir
#endif


  excludeMacrosCorpus (readCorpusFiles' dirs)
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where
    parse = parseByteString @Rust.SourceFile @() tree_sitter_rust
    excludeMacrosCorpus l = liftM (filter (f "expressions") ) l
      where f p bn = p /= takeBaseName bn

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-rust corpus tests"
