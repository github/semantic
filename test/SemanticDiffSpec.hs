module SemanticDiffSpec where

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Prelude
import Prologue (($), fmap, (.), pure, for, panic)
import Test.Hspec (Spec, describe, it, xit, parallel)
import Test.Hspec.Expectations.Pretty
import Data.Text.Lazy.Encoding as E
import Data.Text.Lazy as T
import Data.Map
import qualified Data.Vector as V
import Arguments
import SemanticDiff
import Renderer

spec :: Spec
spec = parallel $ do
  describe "fetchDiffs" $ do
    it "generates diff summaries for two shas" $ do
      (errors, summaries) <- getOutput summaryText $ args "test/fixtures/git/examples/all-languages.git" "dfac8fd681b0749af137aebf3203e77a06fbafc2" "2e4144eb8c44f007463ec34cb66353f0041161fe" ["methods.rb"] Renderer.Summary
      errors `shouldBe` Just (fromList [])
      summaries `shouldBe` Just (fromList [("methods.rb", ["Added the 'foo()' method"])])

    it "generates toc summaries for two shas" $ do
      (errors, summaries) <- getOutput termText $ args "test/fixtures/git/examples/all-languages.git" "dfac8fd681b0749af137aebf3203e77a06fbafc2" "2e4144eb8c44f007463ec34cb66353f0041161fe" ["methods.rb"] Renderer.TOC
      errors `shouldBe` Just (fromList [])
      summaries `shouldBe` Just (fromList [("methods.rb", ["foo"])])

    xit "is ok with bad shas" $ do
      (errors, summaries) <- getOutput summaryText $ args "test/fixtures/git/examples/all-languages.git" "dead" "beef" ["methods.rb"] Renderer.Summary
      errors `shouldBe` Just (fromList [])
      summaries `shouldBe` Just (fromList [("methods.rb", ["Added the 'foo()' method"])])

getOutput :: (Object -> Text) -> Arguments -> IO (Maybe (Map Text Value), Maybe (Map Text [Text]))
getOutput f arguments = do
  diffs <- fetchDiffs arguments
  let json = fromJust . decode . E.encodeUtf8 $ T.fromChunks [concatOutputs diffs]
  pure (errors json, summaries f json)

-- Diff Summaries payloads look like this:
-- {
-- "changes": {
--   "methods.rb": [
--      {
--       "span":{"insert":{"start":[1,1],"end":[2,4]}},
--       "summary":"Added the 'foo()' method"
--      }
--   ]
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
