{-# LANGUAGE FlexibleContexts, RecordWildCards, TypeApplications #-}
{-# OPTIONS_GHC -O1 #-}
module Main (main) where

import           Control.Carrier.Parse.Measured
import           Control.Effect
import           Control.Effect.Reader
import           Control.Exception (displayException)
import qualified Control.Foldl as Foldl
import           Data.Function ((&))
import           Control.Concurrent.Async (forConcurrently)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (ResIO, runResourceT)
import           Data.Blob
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Streaming.Char8 as ByteStream
import           Data.Either
import           Data.Language (defaultLanguageModes)
import           Data.Set (Set)
import           Data.Typeable
import qualified Streaming.Prelude as Stream
import           System.FilePath.Glob
import           System.Path ((</>))
import qualified System.Path as Path
import qualified System.Process as Process

import Data.Flag
import Semantic.Api (TermOutputFormat (..), parseTermBuilder)
import Semantic.Config as Config
import Semantic.Task
import Semantic.Task.Files

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

data LanguageExample
  = LanguageExample
  { languageName             :: String
  , languageExtension        :: String
  , languageExampleDir       :: Path.RelDir
  , languageKnownFailuresTxt :: Maybe Path.RelFile
  } deriving (Eq, Show)

le :: String -> String -> Path.RelDir -> Maybe Path.RelFile -> LanguageExample
le = LanguageExample

examples :: [LanguageExample]
examples =
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

buildExamples :: TaskSession -> LanguageExample -> Path.RelDir -> IO Tasty.TestTree
buildExamples session lang tsDir = do
  knownFailures <- knownFailuresForPath tsDir (languageKnownFailuresTxt lang)
  files <- globDir1 (compile ("**/*" <> languageExtension lang)) (Path.toString (tsDir </> languageExampleDir lang))
  let paths = Path.relFile <$> files
  trees <- forConcurrently paths $ \file -> pure $ HUnit.testCase (Path.toString file) $ do
    res <- runTask session (runParse (parseFilePath file))
    case res of
      Left (SomeException e) -> case cast e of
        -- We have a number of known assignment timeouts, consider these pending specs instead of failing the build.
        Just AssignmentTimedOut -> pure ()
        Just ParserTimedOut     ->     pure ()
        -- Other exceptions are true failures
        _                       -> HUnit.assertFailure (show (displayException e))
      _ -> if file `elem` knownFailures
              then pure ()
              else (isRight res) HUnit.@? ("Error: " <> either show show res)
  pure (Tasty.testGroup (languageName lang) trees)

testOptions :: Config.Options
testOptions = defaultOptions
  { optionsFailOnWarning = flag FailOnWarning True
  , optionsLogLevel = Nothing
  }

main :: IO ()
main = withOptions testOptions $ \ config logger statter -> do
  void $ Process.system "script/clone-example-repos"

  let session = TaskSession config "-" False logger statter

  allTests <- forConcurrently examples $ \lang@LanguageExample{..} -> do
    let tsLang = Path.relDir ("tree-sitter-" <> languageName)
    let tsDir = Path.relDir "tmp/haskell-tree-sitter" </> tsLang </> Path.relDir "vendor" </> tsLang
    buildExamples session lang tsDir

  Tasty.defaultMain $ Tasty.testGroup "parse-examples" allTests

knownFailuresForPath :: Path.RelDir -> Maybe Path.RelFile -> IO (Set Path.RelFile)
knownFailuresForPath _ Nothing = pure mempty
knownFailuresForPath tsDir (Just path)
  = runResourceT
  ( ByteStream.readFile @ResIO (Path.toString (tsDir </> path))
  & ByteStream.lines
  & ByteStream.denull
  & Stream.mapped ByteStream.toLazy
  & Stream.filter ((/= '#') . BLC.head)
  & Stream.map (Path.relFile . BLC.unpack)
  & Foldl.purely Stream.fold_ Foldl.set
  )


parseFilePath :: (Member (Error SomeException) sig, Member Distribute sig, Member Parse sig, Member Files sig, Member (Reader Config) sig, Carrier sig m, MonadIO m) => Path.RelFile -> m Bool
parseFilePath path = readBlob (fileForTypedPath path) >>= runReader defaultLanguageModes . parseTermBuilder @[] TermShow . pure >>= const (pure True)
