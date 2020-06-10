module Semantic.CLI.Spec (testTree) where

import           Analysis.File
import           Control.Carrier.Parse.Simple
import           Control.Carrier.Reader
import           Data.ByteString.Builder
import Control.Exception
import           Data.Language
import           Semantic.Api hiding (Blob, File)
import           Semantic.Task
import           Serializing.Format
import           System.IO.Unsafe
import           System.Path ((</>))
import qualified System.Path as Path
import qualified System.Path.Bazel as Path
import qualified System.Path.Directory as Path

import SpecHelpers
import Test.Tasty
import Test.Tasty.Golden

-- TODO: Fix this, or throw it out entirely.
testTree :: Path.HasBazel => TestTree
testTree = testGroup "Semantic.CLI" []
-- testTree = testGroup "Semantic.CLI"
--   [ testGroup "parseDiffBuilder" $ fmap testForDiffFixture diffFixtures
--   , testGroup "parseTermBuilder" $ fmap testForParseFixture parseFixtures
--   ]

-- We provide this function to the golden tests so as to have better
-- output when diffing JSON outputs. If you're investigating these
-- tests and find this output hard to read, install the `jd` CLI tool
-- (https://github.com/josephburnett/jd), which will print a detailed
-- summary of the differences between these JSON files.
renderDiff :: String -> String -> [String]
renderDiff ref new = unsafePerformIO $ do
  let check p = do
        exists <- Path.doesFileExist (Path.absFile p)
        unless exists (throwIO (userError ("Can't find path " <> p)))

  check ref
  check new
  useJD <- (Path.hasExtension ".json" (Path.absPath ref) &&) <$> fmap isJust (Path.findExecutable "jd")
  pure $ if useJD
    then ["jd", "-set", ref, new]
    else ["diff", ref, new]
{-# NOINLINE renderDiff #-}

testForDiffFixture :: Path.HasBazel => (String, [BlobPair] -> ParseC TaskC Builder, [(File Language, File Language)], Path.AbsFile) -> TestTree
testForDiffFixture (diffRenderer, runDiff, files, expected) =
  goldenVsStringDiff
    ("diff fixture renders to " <> diffRenderer <> " " <> show files)
    renderDiff
    (Path.toString expected)
    (fmap toLazyByteString . runTaskOrDie $ readBlobPairs (Right files) >>= runDiff)

testForParseFixture :: Path.HasBazel => (String, [Blob] -> ParseC TaskC Builder, [File Language], Path.AbsFile) -> TestTree
testForParseFixture (format, runParse, files, expected) =
  goldenVsStringDiff
    ("parse fixture renders to " <> format)
    renderDiff
    (Path.toString expected)
    (fmap toLazyByteString . runTaskOrDie $ readBlobs (FilesFromPaths files) >>= runParse)

parseFixtures :: Path.HasBazel => [(String, [Blob] -> ParseC TaskC Builder, [File Language], Path.AbsFile)]
parseFixtures =
  [ ("s-expression", run . parseTermBuilder TermSExpression, path, Path.bazelFile "test/fixtures/ruby/corpus/and-or.parseA.txt")
  , ("json", run . parseTermBuilder TermJSONTree, path, prefix </> Path.file "parse-tree.json")
  , ("json", run . parseTermBuilder TermJSONTree, path', prefix </> Path.file "parse-trees.json")
  , ("json", run . parseTermBuilder TermJSONTree, [], prefix </> Path.file "parse-tree-empty.json")
  , ("symbols", run . parseSymbolsBuilder Serializing.Format.JSON, path'', prefix </> Path.file "parse-tree.symbols.json")
  , ("protobuf symbols", run . parseSymbolsBuilder Serializing.Format.Proto, path'', prefix </> Path.file "parse-tree.symbols.protobuf.bin")
  ]
  where path = [File (Path.bazelFile' "test/fixtures/ruby/corpus/and-or.A.rb") lowerBound Ruby]
        path' = [File (Path.bazelFile' "test/fixtures/ruby/corpus/and-or.A.rb") lowerBound Ruby, File (Path.bazelFile' "test/fixtures/ruby/corpus/and-or.B.rb") lowerBound Ruby]
        path'' = [File (Path.bazelFile' "test/fixtures/ruby/corpus/method-declaration.A.rb") lowerBound Ruby]
        prefix = Path.bazelDir "test/fixtures/cli"
        run = runReader defaultLanguageModes

diffFixtures :: Path.HasBazel => [(String, [BlobPair] -> ParseC TaskC Builder, [(File Language, File Language)], Path.AbsFile)]
diffFixtures =
  [ ("json diff", parseDiffBuilder DiffJSONTree, pathMode, prefix </> Path.file "diff-tree.json")
  , ("s-expression diff", parseDiffBuilder DiffSExpression, pathMode, Path.bazelFile "test/fixtures/ruby/corpus/method-declaration.diffA-B.txt")
  ]
  where pathMode = [(File (Path.bazelFile' "test/fixtures/ruby/corpus/method-declaration.A.rb") lowerBound Ruby, File (Path.bazelFile' "test/fixtures/ruby/corpus/method-declaration.B.rb") lowerBound Ruby)]
        prefix = Path.bazelDir "test/fixtures/cli"
