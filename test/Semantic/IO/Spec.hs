module Semantic.IO.Spec (spec) where

import Prelude hiding (readFile)

import Control.Concurrent.Async
import Foreign
import Foreign.C.Types (CBool (..))
import Semantic.IO
import System.Exit (ExitCode (..))
import System.IO (IOMode (..))
import Parsing.TreeSitter
import System.Timeout

import qualified TreeSitter.Language as TS
import qualified TreeSitter.Node as TS
import qualified TreeSitter.Parser as TS
import qualified TreeSitter.Tree as TS

import Data.Blob
import Data.Handle
import SpecHelpers hiding (readFile)


spec :: Spec
spec = parallel $ do
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
      putStrLn "step 1"
      blobs <- blobsFromFilePath "test/fixtures/cli/diff.json"
      putStrLn "done"
      blobs `shouldBe` [blobPairDiffing a b]

    it "returns blobs when there's no before" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-no-before.json"
      blobs `shouldBe` [blobPairInserting b]

    it "returns blobs when there's null before" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-null-before.json"
      blobs `shouldBe` [blobPairInserting b]

    it "returns blobs when there's no after" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-no-after.json"
      blobs `shouldBe` [blobPairDeleting a]

    it "returns blobs when there's null after" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-null-after.json"
      blobs `shouldBe` [blobPairDeleting a]


    it "returns blobs for unsupported language" $ do
      h <- openFileForReading "test/fixtures/cli/diff-unsupported-language.json"
      blobs <- readBlobPairsFromHandle h
      let b' = sourceBlob "test.kt" Unknown "fun main(args: Array<String>) {\nprintln(\"hi\")\n}\n"
      blobs `shouldBe` [blobPairInserting b']

    it "detects language based on filepath for empty language" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-empty-language.json"
      blobs `shouldBe` [blobPairDiffing a b]

    it "throws on blank input" $ do
      h <- openFileForReading "test/fixtures/cli/blank.json"
      readBlobPairsFromHandle h `shouldThrow` (== ExitFailure 1)

    it "throws if language field not given" $ do
      h <- openFileForReading "test/fixtures/cli/diff-no-language.json"
      readBlobsFromHandle h `shouldThrow` (== ExitFailure 1)

    it "throws if null on before and after" $ do
      h <- openFileForReading "test/fixtures/cli/diff-null-both-sides.json"
      readBlobPairsFromHandle h `shouldThrow` (== ExitFailure 1)

  describe "cancelable parsing" $
    it "should be cancelable asynchronously" $ do
      p <- TS.ts_parser_new

      churn <- async $ do
        TS.ts_parser_loop_until_cancelled p nullPtr nullPtr 0
        pure True

      res <- timeout 2500 (wait churn)
      res `shouldBe` Nothing

      TS.ts_parser_set_enabled p (CBool 0)
      done <- timeout 2500 (wait churn)

      done `shouldBe` (Just True)

      TS.ts_parser_delete p

  describe "readBlobsFromHandle" $ do
    it "returns blobs for valid JSON encoded parse input" $ do
      h <- openFileForReading "test/fixtures/cli/parse.json"
      blobs <- readBlobsFromHandle h
      let a = sourceBlob "method.rb" Ruby "def foo; end"
      blobs `shouldBe` [a]

    it "throws on blank input" $ do
      h <- openFileForReading "test/fixtures/cli/blank.json"
      readBlobsFromHandle h `shouldThrow` (== ExitFailure 1)

  where blobsFromFilePath path = do
          h <- openFileForReading path
          putStrLn "got handle"
          blobs <- readBlobPairsFromHandle h
          putStrLn "got blobs"
          pure blobs
