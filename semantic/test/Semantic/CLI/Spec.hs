{-# OPTIONS_GHC -Wno-unused-imports #-}

module Semantic.CLI.Spec (testTree) where

import           Analysis.File
import           Analysis.Reference
import           Control.Carrier.Parse.Simple
import           Control.Carrier.Reader
import           Control.Exception
import           Data.ByteString.Builder
import           Semantic.Api hiding (Blob, File)
import           Semantic.Task
import           Serializing.Format
import           Source.Language
import           SpecHelpers
import           System.Directory
import           System.FilePath
import           System.IO.Unsafe
import qualified System.Path.Fixture as Fixture
import           Test.Tasty
import           Test.Tasty.Golden

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
  let check p = do
        exists <- doesFileExist p
        unless exists (throwIO (userError ("Can't find path " <> p)))

  check ref
  check new
  useJD <- (takeExtension ref == ".json" &&) <$> fmap isJust (findExecutable "jd")
  pure $ if useJD
    then ["jd", "-set", ref, new]
    else ["diff", ref, new]
{-# NOINLINE renderDiff #-}


testForParseFixture :: (String, [Blob] -> ParseC TaskC Builder, [File Language], FilePath) -> TestTree
testForParseFixture (format, runParse, files, expected) =
  goldenVsStringDiff
    ("parse fixture renders to " <> format)
    renderDiff
    expected
    (fmap toLazyByteString . runTaskOrDie $ readBlobs (FilesFromPaths files) >>= runParse)

parseFixtures :: [(String, [Blob] -> ParseC TaskC Builder, [File Language], FilePath)]
parseFixtures =
  [ ("s-expression", parseTermBuilder TermSExpression, path, "semantic/test/fixtures/ruby/corpus/and-or.parseA.txt")
  , ("symbols", parseSymbolsBuilder Serializing.Format.JSON, path'', "semantic/test/fixtures/cli/parse-tree.symbols.json")
  , ("protobuf symbols", parseSymbolsBuilder Serializing.Format.Proto, path'', "semantic/test/fixtures/cli/parse-tree.symbols.protobuf.bin")
  ]
  where path = [File (Reference "semantic/test/fixtures/ruby/corpus/and-or.A.rb" lowerBound) Ruby]
        path'' = [File (Reference "semantic/test/fixtures/ruby/corpus/method-declaration.A.rb" lowerBound) Ruby]
