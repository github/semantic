{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Semantic.IO.Spec (spec) where

import Prelude hiding (readFile)

import           Analysis.File as File
import           Analysis.Reference as Ref
import           Data.Blob as Blob
import           Data.Handle
import           SpecHelpers
import qualified System.Path.Fixture as Fixture

spec :: Fixture.HasFixture => Spec
spec = do
#if BAZEL_BUILD
  rf <- runIO Fixture.create
  let ?project = "semantic"
      ?runfiles = rf
#endif
  let blobsFromFilePath path = do
        h <- openFileForReading (Fixture.absRelFile path)
        blobs <- readBlobPairsFromHandle h
        pure blobs
  describe "readFile" $ do

    it "returns a blob for extant files" $ do
      let path = Fixture.absRelFile "test/fixtures/cli/diff.json"
      Just blob <- readBlobFromFile (File (Reference path lowerBound) Unknown)
      blobFilePath blob `shouldBe` path

    it "throws for absent files" $ do
      readBlobFromFile (File (Reference "/dev/doesnotexist" lowerBound) Unknown) `shouldThrow` anyIOException

  describe "readBlobPairsFromHandle" $ do
    let a = Blob.fromSource "method.rb" Ruby "def foo; end"
    let b = Blob.fromSource "method.rb" Ruby "def bar(x); end"
    it "returns blobs for valid JSON encoded diff input" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff.json"
      blobs `shouldBe` [Compare a b]

    it "returns blobs when there's no before" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-no-before.json"
      blobs `shouldBe` [Insert b]

    it "returns blobs when there's null before" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-null-before.json"
      blobs `shouldBe` [Insert b]

    it "returns blobs when there's no after" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-no-after.json"
      blobs `shouldBe` [Delete a]

    it "returns blobs when there's null after" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-null-after.json"
      blobs `shouldBe` [Delete a]


    it "returns blobs for unsupported language" $ do
      h <- openFileForReading (Fixture.absRelFile "test/fixtures/cli/diff-unsupported-language.json")
      blobs <- readBlobPairsFromHandle h
      let b' = Blob.fromSource "test.kt" Unknown "fun main(args: Array<String>) {\nprintln(\"hi\")\n}\n"
      blobs `shouldBe` [Insert b']

    it "detects language based on filepath for empty language" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-empty-language.json"
      blobs `shouldBe` [Compare a b]

    it "throws on blank input" $ do
      h <- openFileForReading (Fixture.absRelFile "test/fixtures/cli/blank.json")
      readBlobPairsFromHandle h `shouldThrow` jsonException

    it "throws if language field not given" $ do
      h <- openFileForReading (Fixture.absRelFile "test/fixtures/cli/diff-no-language.json")
      readBlobsFromHandle h `shouldThrow` jsonException

    it "throws if null on before and after" $ do
      h <- openFileForReading (Fixture.absRelFile "test/fixtures/cli/diff-null-both-sides.json")
      readBlobPairsFromHandle h `shouldThrow` jsonException

  describe "readBlobsFromHandle" $ do
    it "returns blobs for valid JSON encoded parse input" $ do
      h <- openFileForReading (Fixture.absRelFile "test/fixtures/cli/parse.json")
      blobs <- readBlobsFromHandle h
      let a = Blob.fromSource "method.rb" Ruby "def foo; end"
      blobs `shouldBe` [a]

    it "throws on blank input" $ do
      h <- openFileForReading (Fixture.absRelFile "test/fixtures/cli/blank.json")
      readBlobsFromHandle h `shouldThrow` jsonException

jsonException :: Selector InvalidJSONException
jsonException = const True
