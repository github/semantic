module Semantic.CLI.Spec (spec) where

import           Control.Monad (when)
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import           Data.Foldable (for_)
import           Semantic.Api hiding (Blob, BlobPair, File)
import           Semantic.CLI
import           Semantic.IO
import           Semantic.Task
import           Serializing.Format
import           System.Directory
import           System.IO.Unsafe
import           System.Process

import SpecHelpers
import Test.Tasty
import Test.Tasty.Golden

spec :: TestTree
spec = testGroup "Semantic.CLI"
  [ testGroup "parseDiffBuilder" $ fmap testForDiffFixture diffFixtures
  , testGroup "parseTermBuilder" $ fmap testForParseFixture parseFixtures
  ]

-- We provide this function to the golden tests so as to have better
-- output when diffing JSON outputs. If you're investigating these
-- tests and find this output hard to read, install the `jd` CLI tool
-- (https://github.com/josephburnett/jd), which will print a detailed
-- summary of the differences between these JSON files.
renderDiff :: String -> String -> [String]
renderDiff ref new = unsafePerformIO $ do
  useJD <- (isExtensionOf ".json" ref &&) <$> fmap isJust (findExecutable "jd")
  pure $ if useJD
    then ["jd", "-set", ref, new]
    else ["git", "diff", ref, new]
{-# NOINLINE renderDiff #-}

testForDiffFixture (diffRenderer, runDiff, files, expected) =
  goldenVsStringDiff
    ("diff fixture renders to " <> diffRenderer <> " " <> show files)
    renderDiff
    expected
    (fmap toLazyByteString . runTaskOrDie $ readBlobPairs (Right files) >>= runDiff)

testForParseFixture (format, runParse, files, expected) =
  goldenVsStringDiff
    ("diff fixture renders to " <> format <> " " <> show files)
    renderDiff
    expected
    (fmap toLazyByteString . runTaskOrDie $ readBlobs (FilesFromPaths files) >>= runParse)

parseFixtures :: [(String, [Blob] -> TaskEff Builder, [File], FilePath)]
parseFixtures =
  [ ("s-expression", parseTermBuilder TermSExpression, path, "test/fixtures/ruby/corpus/and-or.parseA.txt")
  , ("json", parseTermBuilder TermJSONTree, path, prefix </> "parse-tree.json")
  , ("json", parseTermBuilder TermJSONTree, path', prefix </> "parse-trees.json")
  , ("json", parseTermBuilder TermJSONTree, [], prefix </> "parse-tree-empty.json")
  , ("symbols", parseSymbolsBuilder Serializing.Format.JSON, path'', prefix </> "parse-tree.symbols.json")
  ]
  where path = [File "test/fixtures/ruby/corpus/and-or.A.rb" Ruby]
        path' = [File "test/fixtures/ruby/corpus/and-or.A.rb" Ruby, File "test/fixtures/ruby/corpus/and-or.B.rb" Ruby]
        path'' = [File "test/fixtures/ruby/corpus/method-declaration.A.rb" Ruby]
        prefix = "test/fixtures/cli"

diffFixtures :: [(String, [BlobPair] -> TaskEff Builder, [Both File], FilePath)]
diffFixtures =
  [ ("json diff", parseDiffBuilder DiffJSONTree, pathMode, prefix </> "diff-tree.json")
  , ("s-expression diff", parseDiffBuilder DiffSExpression, pathMode, "test/fixtures/ruby/corpus/method-declaration.diffA-B.txt")
  , ("toc summaries diff", diffSummaryBuilder Serializing.Format.JSON, pathMode, prefix </> "diff-tree.toc.json")
  ]
  where pathMode = [Both (File "test/fixtures/ruby/corpus/method-declaration.A.rb" Ruby) (File "test/fixtures/ruby/corpus/method-declaration.B.rb"  Ruby)]
        prefix = "test/fixtures/cli"
