module Command.Spec where

import Command
import Data.Bifunctor
import Data.Functor.Both
import Data.String
import Prologue hiding (readFile)
import Source
import System.FilePath
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "readFile" $ do
    it "returns a blob for extant files" $ do
      blob <- runCommand (readFile "semantic-diff.cabal")
      fmap path blob `shouldBe` Just "semantic-diff.cabal"

    it "returns Nothing for absent files" $ do
      blob <- runCommand (readFile "this file should not exist")
      blob `shouldBe` Nothing

  describe "readFilesAtSHAs" $ do
    it "returns blobs for the specified paths" $ do
      blobs <- runCommand (readFilesAtSHAs repoPath [] ["methods.rb"] (shas methodsFixture))
      blobs `shouldBe` expectedBlobs methodsFixture

    it "returns blobs for all paths if none are specified" $ do
      blobs <- runCommand (readFilesAtSHAs repoPath [] [] (shas methodsFixture))
      blobs `shouldBe` expectedBlobs methodsFixture

    it "returns entries for missing paths" $ do
      blobs <- runCommand (readFilesAtSHAs repoPath [] ["this file should not exist"] (shas methodsFixture))
      blobs `shouldBe` [("this file should not exist", pure Nothing)]

  where repoPath = "test/fixtures/git/examples/all-languages.git"
        methodsFixture = Fixture
          (both "dfac8fd681b0749af137aebf3203e77a06fbafc2" "2e4144eb8c44f007463ec34cb66353f0041161fe")
          [ ("methods.rb", both Nothing (Just methodsBlob)) ]
        methodsBlob = SourceBlob (Source "def foo\nend\n") "ff7bbbe9495f61d9e1e58c597502d152bab1761e" "methods.rb" (Just defaultPlainBlob)

data Fixture = Fixture { shas :: Both String, expectedBlobs :: [(FilePath, Both (Maybe SourceBlob))] }
