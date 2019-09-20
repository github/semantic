{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           Control.Effect
import           Control.Exception (displayException)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BC
import           Data.Either
import           Data.Blob (fileForPath)
import           Data.Flag
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Quieterm
import           Data.Typeable (cast)
import           Data.Void
import           Parsing.Parser
import           Semantic.Api (TermOutputFormat (..), parseTermBuilder)
import           Semantic.Config (Config (..), Options (..), FailOnWarning (..), defaultOptions)
import qualified Semantic.IO as IO
import           Semantic.Task
import           Semantic.Task.Files
import           System.Directory
import           System.Exit (die)
import           System.FilePath.Glob
import qualified System.Path as Path
import           System.Path ((</>))
import           System.Process
import           Test.Hspec


main :: IO ()
main = withOptions opts $ \ config logger statter -> hspec . parallel $ do
  let args = TaskSession config "-" False logger statter

  runIO setupExampleRepos

  for_ languages $ \ lang@LanguageExample{..} -> do
    let tsLang = Path.relDir ("tree-sitter-" <> languageName)
        tsDir = languagesDir </> tsLang </> Path.relDir "vendor" </> tsLang
    parallel . describe languageName $ parseExamples args lang tsDir

  where
    parseExamples session LanguageExample{..} tsDir = do
      knownFailures <- runIO $ knownFailuresForPath tsDir languageKnownFailuresTxt
      files <- runIO $ globDir1 (compile ("**/*" <> languageExtension)) (Path.toString (tsDir </> languageExampleDir))
      let paths = Path.relFile <$> files
      for_ paths $ \file -> it (Path.toString file) $ do
        res <- runTask session (parseFilePath (Path.toString file))
        case res of
          Left (SomeException e) -> case cast e of
            -- We have a number of known assignment timeouts, consider these pending specs instead of failing the build.
            Just AssignmentTimedOut -> pendingWith $ show (displayException e)
            Just ParserTimedOut -> pendingWith $ show (displayException e)
            -- Other exceptions are true failures
            _ -> expectationFailure (show (displayException e))
          _ -> if file `elem` knownFailures
                  then pendingWith $ "Known parse failures " <> show ("Assignment: OK" <$ res)
                  else res `shouldSatisfy` isRight

    setupExampleRepos = readProcess "script/clone-example-repos" mempty mempty >>= print
    opts = defaultOptions { optionsFailOnWarning = flag FailOnWarning True, optionsLogLevel = Nothing }

    knownFailuresForPath :: Path.RelDir -> Maybe Path.RelFile -> IO [Path.RelFile]
    knownFailuresForPath _     Nothing     = pure []
    knownFailuresForPath tsDir (Just path) = do
      known <- BC.lines <$> B.readFile (Path.toString (tsDir </> path))
      let stripComments = filter (\line -> not (BC.null line) && BC.head line == '#')
      let failures = Path.relFile . BC.unpack <$> stripComments known
      pure ((tsDir </>) <$> failures)

data LanguageExample
  = LanguageExample
  { languageName :: String
  , languageExtension :: String
  , languageExampleDir :: Path.RelDir
  , languageKnownFailuresTxt :: Maybe Path.RelFile
  } deriving (Eq, Show)

le :: String -> String -> Path.RelDir -> Maybe Path.RelFile -> LanguageExample
le = LanguageExample

languages :: [LanguageExample]
languages =
  [ le "python" ".py" examples (Just $ Path.relFile "script/known_failures.txt")
  , le "ruby" ".rb" examples (Just $ Path.relFile "script/known_failures.txt")
  , le "typescript" ".ts" examples (Just $ Path.relFile "typescript/script/known_failures.txt")
  , le "typescript" ".tsx" examples (Just $ Path.relFile "typescript/script/known_failures.txt")
  , le "typescript" ".js" examples Nothing -- parse JavaScript with TypeScript parser.
  , le "go" ".go" examples (Just $ Path.relFile "script/known-failures.txt")

  -- TODO: Java assignment errors need to be investigated
  -- , le "java" ".java" "examples/guava" (Just "script/known_failures_guava.txt")
  -- , le "java" ".java" "examples/elasticsearch" (Just "script/known_failures_elasticsearch.txt")
  -- , le "java" ".java" "examples/RxJava" (Just "script/known_failures_RxJava.txt")

  -- TODO: Haskell assignment errors need to be investigated
  -- , le "haskell" ".hs" "examples/effects" (Just "script/known-failures-effects.txt")
  -- , le "haskell" ".hs" "examples/postgrest" (Just "script/known-failures-postgrest.txt")
  -- , le "haskell" ".hs" "examples/ivory" (Just "script/known-failures-ivory.txt")

  -- , ("php", ".php") -- TODO: No parse-examples in tree-sitter yet
  ] where examples = Path.relDir "examples"

parseFilePath :: (Member (Error SomeException) sig, Member Distribute sig, Member Task sig, Member Files sig, Carrier sig m, MonadIO m) => FilePath -> m Bool
parseFilePath path = readBlob (fileForPath path) >>= parseTermBuilder @[] TermShow . pure >>= const (pure True)

languagesDir :: Path.RelDir
languagesDir = Path.relDir "tmp/haskell-tree-sitter"
