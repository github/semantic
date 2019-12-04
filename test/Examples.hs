{-# LANGUAGE FlexibleContexts, RecordWildCards, TypeApplications #-}
{-# OPTIONS_GHC -O1 #-}
module Main (main) where

import           Control.Carrier.Parse.Measured
import           Control.Concurrent.Async (forConcurrently)
import           Control.Effect
import           Control.Effect.Reader
import           Control.Exception (displayException)
import qualified Control.Foldl as Foldl
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Resource (ResIO, runResourceT)
import           Data.Blob
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Streaming.Char8 as ByteStream
import           Data.Foldable
import           Data.Either
import           Data.Function ((&))
import           Data.Language (LanguageMode (..), PerLanguageModes (..))
import           Data.List
import           Data.Set (Set)
-- import qualified Data.Set as Set
import qualified Streaming.Prelude as Stream
import           System.FilePath.Glob
import           System.Path ((</>))
import qualified System.Path as Path
import qualified System.Process as Process

import Data.Flag
import Proto.Semantic as P hiding (Blob, BlobPair)
import Proto.Semantic_Fields as P
import Semantic.Api.Symbols (parseSymbols)
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
  -- , le "ruby" ".rb" examples (Just $ Path.relFile "script/known_failures.txt")
  -- , le "typescript" ".ts" examples (Just $ Path.relFile "typescript/script/known_failures.txt")
  -- , le "typescript" ".tsx" examples (Just $ Path.relFile "typescript/script/known_failures.txt")
  -- , le "typescript" ".js" examples Nothing -- parse JavaScript with TypeScript parser.
  -- , le "go" ".go" examples (Just $ Path.relFile "script/known-failures.txt")

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
  trees <- forConcurrently paths $ \file -> do
    pure . HUnit.testCaseSteps ("[" <> languageName lang <> "] " <> Path.toString file) $ \step -> do
      -- Use alacarte language mode (this is the control)
      step "a la carte"
      alacarte <- runTask session (runParse (parseSymbolsFilePath aLaCarteLanguageModes file))
      HUnit.assertBool "a la carte parsing failed" (isRight alacarte)

      -- Test out precise language mode (treatment)
      step "precise"
      precise <- runTask session (runParse (parseSymbolsFilePath preciseLanguageModes file))
      HUnit.assertBool "precise parsing failed" (isRight precise)

      step "compare"
      assertMatch file knownFailures alacarte precise

  pure (Tasty.testGroup (languageName lang) trees)

  where
    assertMatch _ _ a b = case (a, b) of
      (Right a, Right b) -> case (toList (a^.files), toList (b^.files)) of
        ([x], _) | (e:_) <- toList (x^.errors) -> HUnit.assertFailure (show e)
        (_, [y]) | (e:_) <- toList (y^.errors) -> HUnit.assertFailure (show e)
        ([x], [y]) -> do
          HUnit.assertEqual "Expected paths to be equal" (x^.path) (y^.path)
          let xSymbols = sort $ toListOf (symbols . traverse . symbol) x
          let ySymbols = sort $ toListOf (symbols . traverse . symbol) y
          HUnit.assertEqual "Expected symbols to be equal" xSymbols ySymbols
          pure ()
        _   -> HUnit.assertFailure "Expected 1 file in each response"
      (Left e, _) -> HUnit.assertFailure (show (displayException e))
      (_, Left e) -> HUnit.assertFailure (show (displayException e))
    -- assertOK res path knownFailures = case res of
    --   Left e    -> if Set.member path knownFailures then pure() else HUnit.assertFailure (show (displayException e))
    --   Right res -> case toList (res^.files) of
    --     [x] | (e:_) <- toList (x^.errors) -> HUnit.assertFailure (show e)
    --     [_] -> pure ()
    --     _   -> HUnit.assertFailure "Expected 1 file in response"

aLaCarteLanguageModes :: PerLanguageModes
aLaCarteLanguageModes = PerLanguageModes
  { pythonMode = ALaCarte
  }

preciseLanguageModes :: PerLanguageModes
preciseLanguageModes = PerLanguageModes
  { pythonMode = Precise
  }

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

parseSymbolsFilePath ::
  ( Member (Error SomeException) sig
  , Member Distribute sig
  , Member Parse sig
  , Member Files sig
  , Carrier sig m
  )
  => PerLanguageModes
  -> Path.RelFile
  -> m ParseTreeSymbolResponse
parseSymbolsFilePath languageModes path
  = readBlob (fileForTypedPath path)
  >>= runReader languageModes . parseSymbols . pure @[]
  -- >>= serialize Format.JSON
  -- >>= pure . toLazyByteString
