{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications, ImplicitParams #-}
module Main (main) where


import           TreeSitter.TSX
import           AST.TestHelpers
import           AST.Unmarshal
import qualified Language.TSX.AST as Tsx
import qualified System.Path as Path
import           Test.Tasty
import qualified Bazel.Runfiles as Runfiles
import qualified System.Path.Fixture as Fixture

main :: IO ()
main = do
  rf <- Runfiles.create
  -- dirs <- Path.absDir <$> Typescript.getTestCorpusDir
  let ?project = Path.relDir "external/semantic-typescript"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "tsx/corpus"

  readCorpusFiles' dirs
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where parse = parseByteString @Tsx.Program @() tree_sitter_tsx

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-tsx corpus tests"
