{-# LANGUAGE FlexibleContexts, RecordWildCards, OverloadedStrings, TypeApplications #-}
{-# OPTIONS_GHC -O1 #-}
module Main (main) where

import           Control.Carrier.Parse.Measured
import           Control.Carrier.Reader
import           Control.Concurrent.Async (forConcurrently)
import           Control.Exception (displayException)
import qualified Control.Foldl as Foldl
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Resource (ResIO, runResourceT)
import           Data.Blob
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Streaming.Char8 as ByteStream
import           Data.Foldable
import           Data.Function ((&))
import           Data.Language (LanguageMode (..), PerLanguageModes (..))
import           Data.List
import qualified Data.Text as Text
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Traversable
import qualified Streaming.Prelude as Stream
import           System.FilePath.Glob
import           System.Path ((</>))
import qualified System.Path as Path
import qualified System.Process as Process
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

import Data.Flag
import Proto.Semantic as P hiding (Blob, BlobPair)
import Proto.Semantic_Fields as P
import Semantic.Api.Symbols (parseSymbols)
import Semantic.Config as Config
import Semantic.Task
import Semantic.Task.Files

data LanguageExample
  = LanguageExample
  { languageName             :: String
  , languageExtension        :: String
  , languageKnownFailuresTxt :: Maybe Path.RelFile
  } deriving (Eq, Show)

le :: String -> String -> Maybe Path.RelFile -> LanguageExample
le = LanguageExample

examples :: [LanguageExample]
examples =
  -- [ le "python" "**/*.py" Nothing -- (Just $ Path.relFile "script/known_failures.txt")
  [ le "ruby" "**/*.rb" Nothing -- (Just $ Path.relFile "script/known_failures.txt")
  -- , le "typescript" "**/*.[jt]s*" Nothing -- (Just $ Path.relFile "typescript/script/known_failures.txt")
  -- , le "typescript" "**/*.tsx" Nothing
  -- , le "javascript" ".js" examples Nothing -- parse JavaScript with TypeScript parser.
  -- , le "go" ".go" examples (Just $ Path.relFile "script/known-failures.txt")

  -- TODO: Java assignment errors need to be investigated
  -- , le "java" ".java" examples (Just $ Path.relFile "script/known_failures_guava.txt")

  -- TODO: Haskell assignment errors need to be investigated
  -- , le "haskell" ".hs" "examples/effects" (Just "script/known-failures-effects.txt")
  -- , le "haskell" ".hs" "examples/postgrest" (Just "script/known-failures-postgrest.txt")
  -- , le "haskell" ".hs" "examples/ivory" (Just "script/known-failures-ivory.txt")

  -- , ("php", ".php") -- TODO: No parse-examples in tree-sitter yet
  ]-- where examples = Path.relDir "examples"

buildExamples :: TaskSession -> LanguageExample -> Path.RelDir -> IO Tasty.TestTree
buildExamples session lang tsDir = do
  knownFailures <- knownFailuresForPath tsDir (languageKnownFailuresTxt lang)
  files <- globDir1 (compile (languageExtension lang)) (Path.toString tsDir)
  let paths = Path.relFile <$> files
  trees <- for paths $ \file -> do
    pure . HUnit.testCaseSteps (Path.toString file) $ \step -> do
      -- Use alacarte language mode
      step "a la carte"
      alacarte <- runTask session (runParse (parseSymbolsFilePath aLaCarteLanguageModes file))
      assertOK "a la carte" alacarte

      -- Test out precise language mode
      step "precise"
      precise <- runTask session (runParse (parseSymbolsFilePath preciseLanguageModes file))
      assertOK "precise" precise

      -- Compare the two
      step "compare"
      assertMatch file knownFailures alacarte precise

  pure (Tasty.testGroup (languageName lang) trees)

  where
    assertOK msg = either (\e -> HUnit.assertFailure (msg <> " failed to parse" <> show e)) (refuteErrors msg)
    refuteErrors msg a = case toList (a^.files) of
      [x] | (e:_) <- toList (x^.errors) -> HUnit.assertFailure (msg <> " parse errors " <> show e)
      _ -> pure ()

    assertMatch filePath knownFailures a b = case (a, b) of
      (Right a, Right b) -> case (toList (a^.files), toList (b^.files)) of
        ([x], [y]) | e1:_ <- toList (x^.errors)
                   , e2:_ <- toList (y^.errors)
                   -> if Set.member filePath knownFailures
                      then pure ()
                      else HUnit.assertFailure ("Parse errors (both) " <> show e1 <> show e2)
        (_, [y])   | e:_ <- toList (y^.errors)
                   -> HUnit.assertFailure ("Parse errors (precise) " <> show e)
        ([x], _)   | e:_ <- toList (x^.errors)
                   -> HUnit.assertFailure ("Parse errors (a la carte) " <> show e)
        ([x], [y]) -> do
          HUnit.assertEqual "Expected paths to be equal" (x^.path) (y^.path)
          let aLaCarteSymbols = sort . filterALaCarteSymbols (languageName lang) $ toListOf (symbols . traverse . symbol) x
              preciseSymbols = sort $ toListOf (symbols . traverse . symbol) y
              delta = aLaCarteSymbols \\ preciseSymbols
              msg = "Found in a la carte, but not precise: "
                  <> show delta
                  <> "\n"
                  <> "Found in precise but not a la carte: "
                  <> show (preciseSymbols \\ aLaCarteSymbols)
                  <> "\n"
                  <> "Expected: " <> show aLaCarteSymbols <> "\n"
                  <> "But got:" <> show preciseSymbols

          HUnit.assertBool ("Expected symbols to be equal.\n" <> msg) (null delta)
          pure ()
        _          -> HUnit.assertFailure "Expected 1 file in each response"
      (Left e1, Left e2) -> HUnit.assertFailure ("Unable to parse (both)" <> show (displayException e1) <> show (displayException e2))
      (_, Left e)        -> HUnit.assertFailure ("Unable to parse (precise)" <> show (displayException e))
      (Left e, _)        -> HUnit.assertFailure ("Unable to parse (a la carte)" <> show (displayException e))


filterALaCarteSymbols :: String -> [Text.Text] -> [Text.Text]
filterALaCarteSymbols "ruby" symbols
  -- = filterBlanks
  -- . removeSetterEqualSign
  = filterOutInstanceVariables
  . filterOutBuiltInMethods
  $ symbols
  where
    -- filterBlanks = filter (not . Text.null)
    -- removeSetterEqualSign = fmap (Text.dropWhileEnd (== '='))
    filterOutInstanceVariables = filter (not . Text.isPrefixOf "@")
    filterOutBuiltInMethods = filter (`notElem` blacklist)
    blacklist =
      [ "alias"
      , "load"
      , "require_relative"
      , "require"
      , "super"
      , "undef"
      , "defined?"
      ]
filterALaCarteSymbols _      symbols = symbols

aLaCarteLanguageModes :: PerLanguageModes
aLaCarteLanguageModes = PerLanguageModes
  { pythonMode = ALaCarte
  , rubyMode = ALaCarte
  -- , typescriptMode = ALaCarte
  }

preciseLanguageModes :: PerLanguageModes
preciseLanguageModes = PerLanguageModes
  { pythonMode = Precise
  , rubyMode = Precise
  -- , typescriptMode = Precise
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
    let tsDir = Path.relDir "tmp" </> Path.relDir (languageName <> "-examples")
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
  ( Has (Error SomeException) sig m
  , Has Distribute sig m
  , Has Parse sig m
  , Has Files sig m
  )
  => PerLanguageModes
  -> Path.RelFile
  -> m ParseTreeSymbolResponse
parseSymbolsFilePath languageModes path = readBlob (fileForTypedPath path) >>= runReader languageModes . parseSymbols . pure @[]
