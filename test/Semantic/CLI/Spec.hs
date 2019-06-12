module Semantic.CLI.Spec (spec) where

import           Control.Monad (when)
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import           Data.Foldable (for_)
import           Semantic.Api hiding (File, Blob, BlobPair)
import           Semantic.CLI
import           Semantic.IO
import           Semantic.Task
import           Serializing.Format

import SpecHelpers
import Test.Tasty
import Test.Tasty.Golden

spec :: TestTree
spec = testGroup "Semantic.CLI"
  [ testGroup "parseDiffBuilder" $ fmap testForDiffFixture diffFixtures
  , testGroup "parseTermBuilder" $ fmap testForParseFixture parseFixtures
  ]

-- PT TODO: reduce duplication

testForDiffFixture (diffRenderer, runDiff, files, expected) =
  goldenVsStringDiff
    ("renders to " <> diffRenderer)
    (\ref new -> ["git", "diff", ref, new])
    expected
    (fmap toLazyByteString . runTaskOrDie $ readBlobPairs (Right files) >>= runDiff)

testForParseFixture (format, runParse, files, expected) =
  goldenVsStringDiff
    ("renders to " <> format)
    (\ref new -> ["git", "diff", ref, new])
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
