{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Main (main) where

import           AST.Test
import           AST.Unmarshal
import qualified Language.Rust.AST as Rust
import           Language.Rust.Grammar
import qualified System.Path as Path
import           Test.Tasty

main :: IO ()
main
  =   Path.absDir <$> Rust.getTestCorpusDir
  >>= readCorpusFiles'
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where
    parse = parseByteString @Rust.Program @() tree_sitter_rust

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-rust corpus tests"
