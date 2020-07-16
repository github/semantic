{-# OPTIONS_GHC -Wno-unused-imports #-}

module Semantic.CLI.Spec (testTree) where

import           Analysis.File
import           Control.Carrier.Parse.Simple
import           Control.Carrier.Reader
import           Data.ByteString.Builder
import Control.Exception
import           Source.Language
import           Semantic.Api hiding (Blob, File)
import           Semantic.Task
import           Serializing.Format
import           System.IO.Unsafe
import qualified System.Path as Path
import qualified System.Path.Fixture as Fixture
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
  let check p = do
        exists <- Path.doesFileExist (Path.absRel p)
        unless exists (throwIO (userError ("Can't find path " <> p)))

  check ref
  check new
  useJD <- (Path.hasExtension ".json" (Path.absRel ref) &&) <$> fmap isJust (Path.findExecutable "jd")
  pure $ if useJD
    then ["jd", "-set", ref, new]
    else ["diff", ref, new]
{-# NOINLINE renderDiff #-}


testForParseFixture :: (String, [Blob] -> ParseC TaskC Builder, [File Language], Path.AbsRelFile) -> TestTree
testForParseFixture (format, runParse, files, expected) =
  goldenVsStringDiff
    ("parse fixture renders to " <> format)
    renderDiff
    (Path.toString expected)
    (fmap toLazyByteString . runTaskOrDie $ readBlobs (FilesFromPaths files) >>= runParse)

parseFixtures :: [(String, [Blob] -> ParseC TaskC Builder, [File Language], Path.AbsRelFile)]
parseFixtures =
  [ ("s-expression", parseTermBuilder TermSExpression, path, Path.absRel "semantic/test/fixtures/ruby/corpus/and-or.parseA.txt")
  , ("symbols", parseSymbolsBuilder Serializing.Format.JSON, path'', Path.absRel "semantic/test/fixtures/cli/parse-tree.symbols.json")
  , ("protobuf symbols", parseSymbolsBuilder Serializing.Format.Proto, path'', Path.absRel "semantic/test/fixtures/cli/parse-tree.symbols.protobuf.bin")
  ]
  where path = [File (Path.absRel "semantic/test/fixtures/ruby/corpus/and-or.A.rb") lowerBound Ruby]
        path'' = [File (Path.absRel "semantic/test/fixtures/ruby/corpus/method-declaration.A.rb") lowerBound Ruby]
