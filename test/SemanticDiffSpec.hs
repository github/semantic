module SemanticDiffSpec where

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Prelude (show, print, traverse, mapM, IO)
import Prologue (($), (<>), (<$>), (=<<), fmap, (.), flip, pure, for, panic)
import Test.Hspec (Spec, describe, it, xit, SpecWith, runIO, parallel, pending)
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
      (errors, summaries) <- getSummaryOutput $ args "test/fixtures/git/examples/ruby.git" "03d0b4db2c6f47c1bb440b9d886a77671e8db340" "5cf7a3421535e139c9a9e47f823db037df3248f6" ["methods.rb"] Renderer.Summary
      errors `shouldBe` Just (fromList [])
      summaries `shouldBe` Just (fromList [("methods.rb", ["Added the 'foo()' method"])])

    it "generates toc summaries for two shas" $ do
      (errors, summaries) <- getTOCOutput $ args "test/fixtures/git/examples/ruby.git" "03d0b4db2c6f47c1bb440b9d886a77671e8db340" "5cf7a3421535e139c9a9e47f823db037df3248f6" ["methods.rb"] Renderer.TOC
      errors `shouldBe` Just (fromList [])
      summaries `shouldBe` Just (fromList [("methods.rb", ["foo"])])

    xit "is ok with bad shas" $ do
      (errors, summaries) <- getSummaryOutput $ args "test/fixtures/git/examples/ruby.git" "dead" "beef" ["methods.rb"] Renderer.Summary
      errors `shouldBe` Just (fromList [])
      summaries `shouldBe` Just (fromList [("methods.rb", ["Added the 'foo()' method"])])

getSummaryOutput :: Arguments -> IO (Maybe (Map Text Value), Maybe (Map Text [Text]))
getSummaryOutput arguments = do
  diffs <- fetchDiffs arguments
  let json = fromJust . decode . E.encodeUtf8 $ T.fromChunks [concatOutputs diffs]
  pure (errors json, summaries summaryText json)

getTOCOutput :: Arguments -> IO (Maybe (Map Text Value), Maybe (Map Text [Text]))
getTOCOutput arguments = do
  diffs <- fetchDiffs arguments
  let json = fromJust . decode . E.encodeUtf8 $ T.fromChunks [concatOutputs diffs]
  pure (errors json, summaries termText json)

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

textForKey key o = fromMaybe (panic "key '" <> key <> "' not found") $
  parseMaybe (.: key) o

errors :: Object -> Maybe (Map Text Value)
errors = parseMaybe (.: "errors")
