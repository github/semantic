module Semantic.IO.Spec (spec) where

import Prelude hiding (readFile)

import Control.Monad.IO.Class
import Data.List
import System.Directory
import System.Exit (ExitCode (..))
import System.IO.Temp
import System.Process
import Control.Exception

import Data.Blob
import Data.Handle
import SpecHelpers hiding (readFile, shouldThrow)
import qualified Semantic.Git as Git

import Test.Tasty
import Test.Tasty.HUnit

shouldThrow :: Exception e => IO a -> (e -> Bool) -> Assertion
shouldThrow act pred = void act `catch` (\e -> pred e @? "didn't pass predicate")

spec :: TestTree
spec = testGroup "Semantic.IO"
  [ testCase "readBlobsFromGitRepo" $ do
      hasGit <- isJust <$> findExecutable "git"
      when hasGit $ do
        -- This temporary directory will be cleaned after use.
        blobs <- liftIO . withSystemTempDirectory "semantic-temp-git-repo" $ \dir -> do
          let commands = [ "cd " <> dir
                         , "git init"
                         , "touch foo.py bar.rb"
                         , "git add foo.py bar.rb"
                         , "git config user.name 'Test'"
                         , "git config user.email 'test@test.test'"
                         , "git commit -am 'test commit'"
                         ]
          exit <- system (intercalate " && " commands)
          when (exit /= ExitSuccess) (fail ("Couldn't run git properly in dir " <> dir))
          readBlobsFromGitRepo (dir </> ".git") (Git.OID "HEAD") []
        let files = sortOn fileLanguage (blobFile <$> blobs)
        files @?= [ File "foo.py" Python
                         , File "bar.rb" Ruby
                         ]

  , testGroup "readFile"
    [ testCase "returns a blob for extant files" $ do
      Just blob <- readBlobFromFile (File "semantic.cabal" Unknown)
      blobPath blob @?= "semantic.cabal"

    , testCase "throws for absent files" $
        readBlobFromFile (File "this file should not exist" Unknown) `shouldThrow` const @_ @IOException True

    ]

  , testGroup "readBlobPairsFromHandle" $
    let a = sourceBlob "method.rb" Ruby "def foo; end"
        b = sourceBlob "method.rb" Ruby "def bar(x); end"
    in [ testCase "returns blobs for valid JSON encoded diff input" $ do
           putStrLn "step 1"
           blobs <- blobsFromFilePath "test/fixtures/cli/diff.json"
           putStrLn "done"
           blobs @?= [Diffing a b]

       , testCase "returns blobs when there's no before" $ do
           blobs <- blobsFromFilePath "test/fixtures/cli/diff-no-before.json"
           blobs @?= [Inserting b]

       , testCase "returns blobs when there's null before" $ do
           blobs <- blobsFromFilePath "test/fixtures/cli/diff-null-before.json"
           blobs @?= [Inserting b]

       , testCase "returns blobs when there's no after" $ do
           blobs <- blobsFromFilePath "test/fixtures/cli/diff-no-after.json"
           blobs @?= [Deleting a]

       , testCase "returns blobs when there's null after" $ do
           blobs <- blobsFromFilePath "test/fixtures/cli/diff-null-after.json"
           blobs @?= [Deleting a]


       , testCase "returns blobs for unsupported language" $ do
           h <- openFileForReading "test/fixtures/cli/diff-unsupported-language.json"
           blobs <- readBlobPairsFromHandle h
           let b' = sourceBlob "test.kt" Unknown "fun main(args: Array<String>) {\nprintln(\"hi\")\n}\n"
           blobs @?= [Inserting b']

       , testCase "detects language based on filepath for empty language" $ do
           blobs <- blobsFromFilePath "test/fixtures/cli/diff-empty-language.json"
           blobs @?= [Diffing a b]

       , testCase "throws on blank input" $ do
           h <- openFileForReading "test/fixtures/cli/blank.json"
           readBlobPairsFromHandle h `shouldThrow` (== ExitFailure 1)

       , testCase "throws if language field not given" $ do
           h <- openFileForReading "test/fixtures/cli/diff-no-language.json"
           readBlobsFromHandle h `shouldThrow` (== ExitFailure 1)

       , testCase "throws if null on before and after" $ do
           h <- openFileForReading "test/fixtures/cli/diff-null-both-sides.json"
           readBlobPairsFromHandle h `shouldThrow` (== ExitFailure 1)
       ]

  , testGroup "readBlobsFromHandle"
    [ testCase "returns blobs for valid JSON encoded parse input" $ do
        h <- openFileForReading "test/fixtures/cli/parse.json"
        blobs <- readBlobsFromHandle h
        let a = sourceBlob "method.rb" Ruby "def foo; end"
        blobs @?= [a]

    , testCase "throws on blank input" $ do
      h <- openFileForReading "test/fixtures/cli/blank.json"
      readBlobsFromHandle h `shouldThrow` (== ExitFailure 1)
    ]
  ]

blobsFromFilePath path = openFileForReading path >>= readBlobPairsFromHandle
