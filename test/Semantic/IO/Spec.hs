{-# LANGUAGE OverloadedStrings #-}

module Semantic.IO.Spec (spec) where

import Prelude hiding (readFile)

import           Data.Blob
import           Data.Handle
import           SpecHelpers

spec :: Spec
spec = do
  describe "readFile" $ do
    it "returns a blob for extant files" $ do
      Just blob <- readBlobFromFile (File "semantic.cabal" Unknown)
      blobPath blob `shouldBe` "semantic.cabal"

    it "throws for absent files" $ do
      readBlobFromFile (File "this file should not exist" Unknown) `shouldThrow` anyIOException

  describe "readBlobPairsFromHandle" $ do
    let a = sourceBlob "method.rb" Ruby "def foo; end"
    let b = sourceBlob "method.rb" Ruby "def bar(x); end"
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
      h <- openFileForReading "test/fixtures/cli/diff-unsupported-language.json"
      blobs <- readBlobPairsFromHandle h
      let b' = sourceBlob "test.kt" Unknown "fun main(args: Array<String>) {\nprintln(\"hi\")\n}\n"
      blobs `shouldBe` [Insert b']

    it "detects language based on filepath for empty language" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-empty-language.json"
      blobs `shouldBe` [Compare a b]

    it "throws on blank input" $ do
      h <- openFileForReading "test/fixtures/cli/blank.json"
      readBlobPairsFromHandle h `shouldThrow` jsonException

    it "throws if language field not given" $ do
      h <- openFileForReading "test/fixtures/cli/diff-no-language.json"
      readBlobsFromHandle h `shouldThrow` jsonException

    it "throws if null on before and after" $ do
      h <- openFileForReading "test/fixtures/cli/diff-null-both-sides.json"
      readBlobPairsFromHandle h `shouldThrow` jsonException

  describe "readBlobsFromHandle" $ do
    it "returns blobs for valid JSON encoded parse input" $ do
      h <- openFileForReading "test/fixtures/cli/parse.json"
      blobs <- readBlobsFromHandle h
      let a = sourceBlob "method.rb" Ruby "def foo; end"
      blobs `shouldBe` [a]

    it "throws on blank input" $ do
      h <- openFileForReading "test/fixtures/cli/blank.json"
      readBlobsFromHandle h `shouldThrow` jsonException

  where blobsFromFilePath path = do
          h <- openFileForReading path
          blobs <- readBlobPairsFromHandle h
          pure blobs

jsonException :: Selector InvalidJSONException
jsonException = const True
