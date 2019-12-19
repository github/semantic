{-# LANGUAGE FlexibleContexts, RecordWildCards, OverloadedStrings, TypeApplications #-}
{-# OPTIONS_GHC -O1 #-}
module Main (main, knownFailuresForPath) where

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
import           Data.Language (LanguageMode (..), PerLanguageModes (..))
import           Data.List
import qualified Data.Text as Text
import           Data.Set (Set)
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

data LanguageExample =
  LanguageExample
    { languageName      :: String
    , languageExtension :: String
    , languageSkips     :: [Path.RelFile]
    , languageDirSkips  :: [Path.RelDir]
    }
  deriving (Eq, Show)

le :: String -> String -> [Path.RelFile] -> [Path.RelDir] -> LanguageExample
le = LanguageExample

examples :: [LanguageExample]
examples =
  [ le "go" "**/*.go" goFileSkips goDirSkips
  -- [ le "python" "**/*.py" mempty mempty
  -- , le "ruby" "**/*.rb" rubySkips mempty
  -- , le "typescript" "**/*.[jt]s*" mempty mempty
  -- , le "typescript" "**/*.tsx" mempty mempty
  ]

goFileSkips :: [Path.RelFile]
goFileSkips = fmap Path.relPath
  [
  -- Super slow
    "go/src/vendor/golang_org/x/text/unicode/norm/tables.go"
  , "go/src/vendor/golang_org/x/text/unicode/bidi/tables.go"
  , "go/src/vendor/golang_org/x/net/idna/tables.go"
  , "go/src/cmd/vendor/golang.org/x/arch/x86/x86asm/tables.go"
  , "moby/vendor/golang.org/x/text/unicode/norm/tables9.0.0.go"
  , "moby/vendor/golang.org/x/text/unicode/norm/tables10.0.0.go"

  -- Assignment timeouts
  , "go/src/cmd/compile/internal/gc/constFold_test.go"
  , "go/src/cmd/compile/internal/gc/testdata/arithConst.go"
  , "moby/vendor/github.com/docker/swarmkit/api/types.pb.go"
  , "moby/vendor/github.com/docker/swarmkit/api/control.pb.go"

  -- Parser timeouts
  , "moby/vendor/github.com/ugorji/go/codec/fast-path.generated.go"

  -- Parse errors
  , "go/src/math/big/arith.go" -- Unhandled identifier character: 'ŝ'
  , "go/src/cmd/vet/testdata/deadcode.go"
  , "moby/vendor/github.com/beorn7/perks/quantile/stream.go" -- Unhandled identifier character: 'ƒ'

  -- UTF8 encoding issues ("Cannot decode byte '\xe3': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream")
  , "go/src/text/template/exec_test.go"
  , "go/src/bufio/bufio_test.go"
  , "go/doc/progs/go1.go"
  ]

goDirSkips :: [Path.RelDir]
goDirSkips = Path.rel <$>
  [ "go/src/cmd/compile/internal/ssa"
  , "go/test/fixedbugs"
  , "go/test/syntax"
  , "go/test/method4.dir"
  , "go/test"
  ]


-- rubySkips :: [Path.RelFile]
-- rubySkips = Path.relFile <$>
--   [
--   -- UTF8 encoding issues ("Cannot decode byte '\xe3': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream")
--   -- These are going to be hard to fix as Ruby allows non-utf8 character content in string literals
--     "ruby_spec/optional/capi/string_spec.rb"
--   , "ruby_spec/core/string/b_spec.rb"
--   , "ruby_spec/core/string/shared/encode.rb"

--   -- Doesn't parse b/c of issue with r<<i
--   , "ruby_spec/core/enumerable/shared/inject.rb"
--   -- Doesn't parse
--   , "ruby_spec/language/string_spec.rb"

--   -- Can't detect method calls inside heredoc bodies with precise ASTs
--   , "ruby_spec/core/argf/readpartial_spec.rb"
--   , "ruby_spec/core/process/exec_spec.rb"
--   ]

buildExamples :: TaskSession -> LanguageExample -> Path.RelDir -> IO Tasty.TestTree
buildExamples session lang tsDir = do
  let fileSkips = fmap (tsDir </>) (languageSkips lang)
      dirSkips  = fmap (tsDir </>) (languageDirSkips lang)
  files <- globDir1 (compile (languageExtension lang)) (Path.toString tsDir)
  let paths = filter (\x -> Path.takeDirectory x `notElem` dirSkips) . filter (`notElem` fileSkips) $ Path.relFile <$> files
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
      assertMatch alacarte precise

  pure (Tasty.testGroup (languageName lang) trees)

  where
    assertOK msg = either (\e -> HUnit.assertFailure (msg <> " failed to parse" <> show e)) (refuteErrors msg)
    refuteErrors msg a = case toList (a^.files) of
      [x] | (e:_) <- toList (x^.errors) -> HUnit.assertFailure (msg <> " parse errors " <> show e)
      _ -> pure ()

    assertMatch a b = case (a, b) of
      (Right a, Right b) -> case (toList (a^.files), toList (b^.files)) of
        ([x], [y]) | e1:_ <- toList (x^.errors)
                   , e2:_ <- toList (y^.errors)
                   -> HUnit.assertFailure ("Parse errors (both) " <> show e1 <> show e2)
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
  = filterOutInstanceVariables
  . filterOutBuiltInMethods
  $ symbols
  where
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
      , "lambda"
      ]
filterALaCarteSymbols _      symbols = symbols

aLaCarteLanguageModes :: PerLanguageModes
aLaCarteLanguageModes = PerLanguageModes
  { pythonMode = ALaCarte
  , rubyMode = ALaCarte
  , goMode = ALaCarte
  }

preciseLanguageModes :: PerLanguageModes
preciseLanguageModes = PerLanguageModes
  { pythonMode = Precise
  , rubyMode = Precise
  , goMode = Precise
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
