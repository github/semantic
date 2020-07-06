{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           AST.TestHelpers
import           AST.Unmarshal (parseByteString)
import           Language.Rust.Grammar
import qualified Language.Rust.AST as Rust
import qualified System.Path as Path
import           Test.Tasty
import Control.Monad (liftM)

main :: IO ()
main = do
#if BAZEL_BUILD
  rf <- Fixture.create
  let ?project = Path.relDir "external/tree-sitter-python"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "test/corpus"
#else
  dirs <- Path.absRel <$> Rust.getTestCorpusDir
#endif


  excludeMacrosCorpus (readCorpusFiles' dirs)
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where
    parse = parseByteString @Rust.SourceFile @() tree_sitter_rust
    excludeMacrosCorpus l = liftM (filter (f "expressions") ) l
      where f p bn = p /= (Path.toString . Path.takeBaseName) bn

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-rust corpus tests"
