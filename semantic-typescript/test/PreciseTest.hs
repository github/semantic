{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications, ImplicitParams #-}
module Main (main) where

import           TreeSitter.TypeScript
import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.TypeScript.AST as Ts
import qualified System.Path as Path
import           Test.Tasty
import qualified Bazel.Runfiles as Runfiles
import qualified System.Path.Fixture as Fixture

main :: IO ()
main = do
  rf <- Runfiles.create
  -- dirs <- Path.absDir <$> Typescript.getTestCorpusDir
  let ?project = Path.relDir "external/tree-sitter-typescript"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "typescript/corpus"

  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where parse = parseByteString @Ts.Program @() tree_sitter_typescript

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-typescript corpus tests"
