module Semantic.IO.Spec (spec) where

import Prelude hiding (readFile)

import Control.Monad.IO.Class
import Data.List
import System.Directory
import System.IO.Temp
import Data.String

import Data.Blob
import Data.Handle
import SpecHelpers hiding (readFile)
import qualified Semantic.Git as Git
import Shelly (shelly, silently, cd, run_)

spec :: Spec
spec = do
  describe "readBlobsFromGitRepo" $ do
    hasGit <- runIO $ isJust <$> findExecutable "git"
    when hasGit . it "should read from a git directory" $ do
      -- This temporary directory will be cleaned after use.
      blobs <- liftIO . withSystemTempDirectory "semantic-temp-git-repo" $ \dir -> do
        shelly $ silently $ do
          cd (fromString dir)
          let git = run_ "git"
          git ["init"]
          run_ "touch" ["foo.py", "bar.rb"]
          git ["add", "foo.py", "bar.rb"]
          git ["config", "user.name", "'Test'"]
          git ["config", "user.email", "'test@test.test'"]
          git ["commit", "-am", "'test commit'"]

        readBlobsFromGitRepo (dir </> ".git") (Git.OID "HEAD") [] []
      let files = sortOn fileLanguage (blobFile <$> blobs)
      files `shouldBe` [ File "foo.py" Python
                       , File "bar.rb" Ruby
                       ]

    when hasGit . it "should read from a git directory with --only" $ do
      -- This temporary directory will be cleaned after use.
      blobs <- liftIO . withSystemTempDirectory "semantic-temp-git-repo" $ \dir -> do
        shelly $ silently $ do
          cd (fromString dir)
          let git = run_ "git"
          git ["init"]
          run_ "touch" ["foo.py", "bar.rb"]
          git ["add", "foo.py", "bar.rb"]
          git ["config", "user.name", "'Test'"]
          git ["config", "user.email", "'test@test.test'"]
          git ["commit", "-am", "'test commit'"]

        readBlobsFromGitRepo (dir </> ".git") (Git.OID "HEAD") [] ["foo.py"]
      let files = sortOn fileLanguage (blobFile <$> blobs)
      files `shouldBe` [ File "foo.py" Python ]

    when hasGit . it "should read from a git directory with --exclude" $ do
      -- This temporary directory will be cleaned after use.
      blobs <- liftIO . withSystemTempDirectory "semantic-temp-git-repo" $ \dir -> do
        shelly $ silently $ do
          cd (fromString dir)
          let git = run_ "git"
          git ["init"]
          run_ "touch" ["foo.py", "bar.rb"]
          git ["add", "foo.py", "bar.rb"]
          git ["config", "user.name", "'Test'"]
          git ["config", "user.email", "'test@test.test'"]
          git ["commit", "-am", "'test commit'"]

        readBlobsFromGitRepo (dir </> ".git") (Git.OID "HEAD") ["foo.py"] []
      let files = sortOn fileLanguage (blobFile <$> blobs)
      files `shouldBe` [ File "bar.rb" Ruby ]

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
      blobs `shouldBe` [Diffing a b]

    it "returns blobs when there's no before" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-no-before.json"
      blobs `shouldBe` [Inserting b]

    it "returns blobs when there's null before" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-null-before.json"
      blobs `shouldBe` [Inserting b]

    it "returns blobs when there's no after" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-no-after.json"
      blobs `shouldBe` [Deleting a]

    it "returns blobs when there's null after" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-null-after.json"
      blobs `shouldBe` [Deleting a]


    it "returns blobs for unsupported language" $ do
      h <- openFileForReading "test/fixtures/cli/diff-unsupported-language.json"
      blobs <- readBlobPairsFromHandle h
      let b' = sourceBlob "test.kt" Unknown "fun main(args: Array<String>) {\nprintln(\"hi\")\n}\n"
      blobs `shouldBe` [Inserting b']

    it "detects language based on filepath for empty language" $ do
      blobs <- blobsFromFilePath "test/fixtures/cli/diff-empty-language.json"
      blobs `shouldBe` [Diffing a b]

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
