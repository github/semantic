{-# LANGUAGE CPP, DisambiguateRecordFields, OverloadedStrings, TypeApplications, ImplicitParams #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where

import           TreeSitter.OCaml
import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.OCaml.AST as Ts
import qualified System.Path as Path
import           Test.Tasty
import qualified System.Path.Fixture as Fixture

main :: IO ()
main = do
#if BAZEL_BUILD
  rf <- Fixture.create
  let ?project = Path.relDir "external/tree-sitter-ocaml"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "ocaml/corpus"
#else
  dirs <- Path.absRel <$> Ts.getTestCorpusDir
#endif

  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where parse = parseByteString @Ts.CompilationUnit @() tree_sitter_ocaml

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-ocaml corpus tests"
