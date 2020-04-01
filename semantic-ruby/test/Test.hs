{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import qualified System.Path as Path
import           Test.Tasty
import           TreeSitter.Ruby
import qualified TreeSitter.Ruby.AST as Rb
import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.Ruby.AST as Rb
import           Language.Ruby.Grammar
import qualified System.Path as Path
import           Test.Tasty

main :: IO ()
main
  =   Path.absDir <$> Rb.getTestCorpusDir
  >>= readCorpusFiles'
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @Rb.Program @() tree_sitter_ruby

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-ruby corpus tests"
