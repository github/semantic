module Main (main) where

import           Control.Monad
import           Control.Exception (displayException)
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BC
import           Data.Foldable
import           Data.Maybe
import           Data.Either
import           Data.List
import           Data.Project (file)
import           Data.Void
import           Rendering.Renderer
import           Semantic.Config (Config (..), Options (..), defaultOptions)
import qualified Semantic.IO as IO
import           Semantic.Parse
import           Semantic.Task
import           Semantic.Util (TaskConfig (..))
import           System.Exit (die)
import           System.FilePath.Glob
import           System.FilePath.Posix
import           System.Directory
import           System.Process
import           Test.Hspec
import           Parsing.Parser
import           Data.Quieterm


main :: IO ()
main = withOptions opts $ \ config logger statter -> hspec . parallel $ do
  let args = TaskConfig config logger statter

  runIO setupExampleRepos

  for_ languages $ \ lang@LanguageExample{..} -> do
    let tsDir = languagesDir </> languageName </> ("vendor/tree-sitter-" <> languageName)
    parallel . describe languageName $ parseExamples args lang tsDir

  where
    parseExamples args LanguageExample{..} tsDir = do
      knownFailures <- runIO $ knownFailuresForPath tsDir languageKnownFailuresTxt
      files <- runIO $ globDir1 (compile ("**/*" <> languageExtension)) (tsDir </> languageExampleDir)
      for_ files $ \file -> it file $ do
        res <- parseFilePath args file
        if file `elem` knownFailures
          then pendingWith $ "Known parse failures " <> show (const "Assignment: OK" <$> res)
          else res `shouldSatisfy` isRight

    setupExampleRepos = readProcess "script/clone-example-repos" mempty mempty >>= print
    opts = defaultOptions { optionsFailOnWarning = True, optionsLogLevel = Nothing }

    knownFailuresForPath :: FilePath -> Maybe FilePath -> IO [FilePath]
    knownFailuresForPath _     Nothing     = pure []
    knownFailuresForPath tsDir (Just path) = do
      known <- BC.lines <$> B.readFile (tsDir </> path)
      pure $ (tsDir </>) . BC.unpack <$> known

data LanguageExample
  = LanguageExample
  { languageName :: FilePath
  , languageExtension :: FilePath
  , languageExampleDir :: FilePath
  , languageKnownFailuresTxt :: Maybe FilePath
  } deriving (Eq, Show)

le :: FilePath -> FilePath -> FilePath -> Maybe FilePath -> LanguageExample
le = LanguageExample

languages :: [LanguageExample]
languages =
  [ le "python" ".py" "examples" (Just "script/known_failures.txt")
  , le "go" ".go" "examples" (Just "script/known-failures.txt")
  , le "ruby" ".rb" "examples" (Just "script/known_failures.txt")
  , le "typescript" ".ts" "examples" (Just "script/known_failures.txt")
  -- , le "java" ".java" "examples/guava" (Just "script/known_failures_guava.txt")
  -- , le "java" ".java" "examples/elasticsearch" (Just "script/known_failures_elasticsearch.txt")
  -- , le "java" ".java" "examples/RxJava" (Just "script/known_failures_RxJava.txt")
  -- , le "haskell" ".hs" "examples/effects" (Just "script/known-failures-effects.txt")
  -- , le "haskell" ".hs" "examples/postgrest" (Just "script/known-failures-postgrest.txt")
  -- , le "haskell" ".hs" "examples/ivory" (Just "script/known-failures-ivory.txt")

  -- , ("javascript", ".js") -- TODO: Actually test javascript
  -- , ("php", ".php") -- TODO: No parse-examples in tree-sitter yet
  ]

parseFilePath :: TaskConfig -> FilePath -> IO (Either String ())
parseFilePath (TaskConfig config logger statter) path = do
  blob <- IO.readFile (file path)
  case blob of
    Just blob -> parse blob
    Nothing -> pure $ Left "readFile failed"
  where parse blob = either (Left . displayException) (const (Right ())) <$> runTaskWithConfig config logger statter (runParse' blob)

languagesDir :: FilePath
languagesDir = "vendor/haskell-tree-sitter/languages"
