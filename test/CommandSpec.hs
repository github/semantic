module CommandSpec where

import Command
import Data.Aeson
import Data.Aeson.Types hiding (parse)
import Data.Functor.Both
import Data.Map
import Data.Maybe
import Data.String
import Info (DefaultFields)
import Language
import Prologue hiding (readFile, toList)
import qualified Data.Vector as V
import qualified Git.Types as Git
import Renderer hiding (errors)
import Source
import Syntax
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty

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

  describe "parse" $ do
    it "parses line by line if not given a language" $ do
      term <- runCommand (parse Nothing methodsBlob)
      void term `shouldBe` cofree (() :< Indexed [ cofree (() :< Leaf "def foo\n"), cofree (() :< Leaf "end\n"), cofree (() :< Leaf "") ])

    it "parses in the specified language" $ do
      term <- runCommand (parse (Just Ruby) methodsBlob)
      void term `shouldBe` cofree (() :< Indexed [ cofree (() :< Method [] (cofree (() :< Leaf "foo")) Nothing [] []) ])

  describe "fetchDiffs" $ do
    it "generates diff summaries for two shas" $ do
      (errors, summaries) <- fetchDiffsOutput summaryText "test/fixtures/git/examples/all-languages.git" "dfac8fd681b0749af137aebf3203e77a06fbafc2" "2e4144eb8c44f007463ec34cb66353f0041161fe" ["methods.rb"] Renderer.SummaryRenderer
      errors `shouldBe` Just (fromList [])
      summaries `shouldBe` Just (fromList [("methods.rb", ["Added the 'foo()' method"])])

    it "generates toc summaries for two shas" $ do
      (errors, summaries) <- fetchDiffsOutput termText "test/fixtures/git/examples/all-languages.git" "dfac8fd681b0749af137aebf3203e77a06fbafc2" "2e4144eb8c44f007463ec34cb66353f0041161fe" ["methods.rb"] Renderer.ToCRenderer
      errors `shouldBe` Just (fromList [])
      summaries `shouldBe` Just (fromList [("methods.rb", ["foo"])])

    it "generates toc summaries for two shas inferring paths" $ do
      (errors, summaries) <- fetchDiffsOutput termText "test/fixtures/git/examples/all-languages.git" "dfac8fd681b0749af137aebf3203e77a06fbafc2" "2e4144eb8c44f007463ec34cb66353f0041161fe" [] Renderer.ToCRenderer
      errors `shouldBe` Just (fromList [])
      summaries `shouldBe` Just (fromList [("methods.rb", ["foo"])])

    it "errors with bad shas" $
      fetchDiffsOutput summaryText "test/fixtures/git/examples/all-languages.git" "dead" "beef" ["methods.rb"] Renderer.SummaryRenderer
        `shouldThrow` (== Git.BackendError "Could not lookup dead: Object not found - no match for prefix (dead000000000000000000000000000000000000)")

    it "errors with bad repo path" $
      fetchDiffsOutput summaryText "test/fixtures/git/examples/not-a-repo.git" "dfac8fd681b0749af137aebf3203e77a06fbafc2" "2e4144eb8c44f007463ec34cb66353f0041161fe" ["methods.rb"] Renderer.SummaryRenderer
        `shouldThrow` errorCall "Could not open repository \"test/fixtures/git/examples/not-a-repo.git\""

  where repoPath = "test/fixtures/git/examples/all-languages.git"
        methodsFixture = Fixture
          (both "dfac8fd681b0749af137aebf3203e77a06fbafc2" "2e4144eb8c44f007463ec34cb66353f0041161fe")
          [ ("methods.rb", both Nothing (Just methodsBlob)) ]
        methodsBlob = SourceBlob (Source "def foo\nend\n") "ff7bbbe9495f61d9e1e58c597502d152bab1761e" "methods.rb" (Just defaultPlainBlob)

data Fixture = Fixture { shas :: Both String, expectedBlobs :: [(FilePath, Both (Maybe SourceBlob))] }

fetchDiffsOutput :: (Object -> Text) -> FilePath -> String -> String -> [FilePath] -> DiffRenderer DefaultFields Summaries -> IO (Maybe (Map Text Value), Maybe (Map Text [Text]))
fetchDiffsOutput f gitDir sha1 sha2 filePaths renderer = do
  results <- fmap encode . runCommand $ do
    blobs <- readFilesAtSHAs gitDir [] filePaths (both sha1 sha2)
    diffs <- for blobs . uncurry $ \ path blobs -> do
      terms <- traverse (traverse parseBlob) blobs
      Just diff' <- maybeDiff terms
      return (fromMaybe <$> pure (emptySourceBlob path) <*> blobs, diff')
    renderDiffs renderer diffs
  let json = fromJust (decode results)
  pure (errors json, summaries f json)

-- Diff Summaries payloads look like this:
-- {
--  "changes": { "methods.rb": [{ "span":{"insert":{"start":[1,1],"end":[2,4]}}, "summary":"Added the 'foo()' method" }] },
-- "errors":{}
-- }
-- TOC Summaries payloads look like this:
-- {
-- "changes": { "methods.rb": [{ "span":{"start":[1,1],"end":[2,4]}, "category":"Method", "term":"foo", "changeType":"added" }]
-- },
-- "errors":{}
-- }
summaries :: (Object -> Text) -> Object -> Maybe (Map Text [Text])
summaries f = parseMaybe $ \o -> do
                changes <- o .: "changes" :: Parser (Map Text (V.Vector Object))
                xs <- for (toList changes) $ \(path, s) -> do
                  let ys = fmap f s
                  pure (path, V.toList ys)
                pure $ fromList xs

summaryText :: Object -> Text
summaryText o = fromMaybe (panic "key 'summary' not found") $
  parseMaybe (.: "summary") o

termText :: Object -> Text
termText o = fromMaybe (panic "key 'term' not found") $
  parseMaybe (.: "term") o

errors :: Object -> Maybe (Map Text Value)
errors = parseMaybe (.: "errors")
