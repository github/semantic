{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           AST.Test
import           AST.Unmarshal (parseByteString)
import qualified Language.Rust.AST as Rust
import           Language.Rust.Grammar
import qualified System.Path as Path
import           Test.Tasty
import Control.Monad (liftM)

main :: IO ()
main
  =   Path.absDir <$> Rust.getTestCorpusDir
  >>= excludeMacrosCorpus . readCorpusFiles'
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where
    parse = parseByteString @Rust.SourceFile @() tree_sitter_rust
    excludeMacrosCorpus l = liftM (filter (f "expressions") ) $ liftM (filter (f "macros") ) l
      where f p bn = p /= (Path.toString . Path.takeBaseName) bn

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-rust corpus tests"