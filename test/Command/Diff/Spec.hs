module Command.Diff.Spec where

import Command
import Data.Aeson
import Data.Aeson.Types
import Data.Functor.Both
import Data.Map as Map
import Data.Maybe
import Data.Text.Lazy as T
import qualified Data.Vector as V
import qualified Git.Types as Git
import Info
import Prelude
import Prologue (($), fmap, (.), pure, for, panic)
import Renderer hiding (errors)
import Source
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = parallel $ do
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

fetchDiffsOutput :: (Object -> Text) -> FilePath -> String -> String -> [FilePath] -> DiffRenderer DefaultFields Summaries -> IO (Maybe (Map Text Value), Maybe (Map Text [Text]))
fetchDiffsOutput f gitDir sha1 sha2 filePaths renderer = do
  results <- fmap encode . runCommand $ do
    blobs <- readFilesAtSHAs gitDir [] filePaths sha1 sha2
    diffs <- for blobs . uncurry $ \ path blobs -> do
      terms <- traverse (traverse parseBlob) blobs
      Just diff' <- runBothWith maybeDiff terms
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
