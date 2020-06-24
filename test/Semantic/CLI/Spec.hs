module Semantic.CLI.Spec (testTree) where

import           Analysis.File
import           Control.Carrier.Parse.Simple
import           Control.Carrier.Reader
import           Data.ByteString.Builder
import           Data.Language
import           Semantic.Api hiding (Blob, File)
import           Semantic.Task
import           Serializing.Format
import           System.IO.Unsafe
import           System.Path ((</>))
import qualified System.Path as Path
import qualified System.Path.Directory as Path

import SpecHelpers
import Test.Tasty
import Test.Tasty.Golden

testTree :: TestTree
testTree = testGroup "Semantic.CLI"
  [ testGroup "parseTermBuilder" $ fmap testForParseFixture parseFixtures
  ]

-- We provide this function to the golden tests so as to have better
-- output when diffing JSON outputs. If you're investigating these
-- tests and find this output hard to read, install the `jd` CLI tool
-- (https://github.com/josephburnett/jd), which will print a detailed
-- summary of the differences between these JSON files.
renderDiff :: String -> String -> [String]
renderDiff ref new = unsafePerformIO $ do
  useJD <- (Path.hasExtension ".json" (Path.relPath ref) &&) <$> fmap isJust (Path.findExecutable "jd")
  pure $ if useJD
    then ["jd", "-set", ref, new]
    else ["git", "diff", ref, new]
{-# NOINLINE renderDiff #-}

testForParseFixture :: (String, [Blob] -> ParseC TaskC Builder, [File Language], Path.RelFile) -> TestTree
testForParseFixture (format, runParse, files, expected) =
  goldenVsStringDiff
    ("parse fixture renders to " <> format)
    renderDiff
    (Path.toString expected)
    (fmap toLazyByteString . runTaskOrDie $ readBlobs (FilesFromPaths files) >>= runParse)

parseFixtures :: [(String, [Blob] -> ParseC TaskC Builder, [File Language], Path.RelFile)]
parseFixtures =
  [ ("s-expression", run . parseTermBuilder TermSExpression, path, Path.relFile "test/fixtures/ruby/corpus/and-or.parseA.txt")
  , ("symbols", run . parseSymbolsBuilder Serializing.Format.JSON, path'', prefix </> Path.file "parse-tree.symbols.json")
  , ("protobuf symbols", run . parseSymbolsBuilder Serializing.Format.Proto, path'', prefix </> Path.file "parse-tree.symbols.protobuf.bin")
  ]
  where path = [File (Path.absRel "test/fixtures/ruby/corpus/and-or.A.rb") lowerBound Ruby]
        path'' = [File (Path.absRel "test/fixtures/ruby/corpus/method-declaration.A.rb") lowerBound Ruby]
        prefix = Path.relDir "test/fixtures/cli"
        run = runReader defaultLanguageModes
