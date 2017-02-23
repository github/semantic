module IntegrationFormatSpec where

import Arguments
import Data.Aeson
import Data.List.Split
import Control.Exception
import qualified Data.ByteString.Lazy as DL
import JSONTestCase
import Test.Hspec (Spec, describe, it, SpecWith, runIO, parallel)
import Prelude
import Prologue
import Renderer
import SemanticDiff
import System.FilePath.Glob
import Data.Maybe (fromJust)
import Test.Hspec.Expectations.Pretty

catchException :: IO [Text] -> IO [Text]
catchException = handle errorHandler
  where errorHandler :: (SomeException -> IO [Text])
        errorHandler exception = return [toS . encode $ ["Crashed: " <> Prologue.show exception :: Text]]

assertDiffSummary :: JSONTestCase -> Format -> (Either String ExpectedResult -> Either String ExpectedResult -> Expectation) -> Expectation
assertDiffSummary JSONTestCase {..} format matcher = do
  diffs <- fetchDiffs $ args gitDir (Prelude.head shas') (Prelude.last shas') filePaths format
  result <- catchException . pure . pure . concatOutputs $ diffs
  let actual = eitherDecode . DL.fromStrict . encodeUtf8 . fromJust . listToMaybe $ result
  matcher actual (Right expectedResult)
  where shas' = splitOn ".." shas

runTestsIn :: [FilePath] -> Format -> (Either String ExpectedResult -> Either String ExpectedResult -> Expectation) -> SpecWith ()
runTestsIn filePaths format matcher = do
  contents <- runIO $ traverse DL.readFile filePaths
  let filePathContents = zip filePaths contents
  let jsonContents = (\(filePath, content) -> (filePath, eitherDecode content)) <$> filePathContents :: [(FilePath, Either String [JSONTestCase])]
  traverse_ handleJSONTestCase jsonContents
  where handleJSONTestCase :: (FilePath, Either String [JSONTestCase]) -> SpecWith ()
        handleJSONTestCase (filePath, eitherJSONTestCase) =
          case eitherJSONTestCase of
            Left err ->  it ("An error occurred " <> err <> " (" <> filePath <> ")") $ True `shouldBe` False
            Right testCases -> traverse_ (\testCase -> it (testCaseDescription testCase) $ assertDiffSummary testCase format matcher) testCases

spec :: Maybe String -> Spec
spec maybeLanguage = parallel $ do
  summaryFormatFiles <- runIO $ testCaseFiles maybeLanguage "test/corpus/diff-summaries"
  summaryFormatToDoFiles <- runIO $ testCaseFiles maybeLanguage "test/corpus/diff-summaries-todo"
  summaryFormatCrasherFiles <- runIO $ testCaseFiles maybeLanguage "test/corpus/diff-summary-crashers"

  describe "Summary format" $ runTestsIn summaryFormatFiles Summary shouldBe
  describe "Summary format todo" $ runTestsIn summaryFormatToDoFiles Summary shouldNotBe
  describe "Summary format crashers todo" $ runTestsIn summaryFormatCrasherFiles Summary shouldBe

  where
    testCaseFiles :: Maybe String -> String -> IO [FilePath]
    testCaseFiles maybeLanguage dir = case maybeLanguage of
      Just language -> globDir1 (compile (language <> "/*.json")) dir
      Nothing -> globDir1 (compile "*/*.json") dir
