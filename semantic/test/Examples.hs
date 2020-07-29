{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -O1 #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-unused-imports #-}
module Main (main) where

import qualified Analysis.File as File
import           Control.Carrier.Parse.Measured
import           Control.Carrier.Reader
import           Control.Concurrent.Async (forConcurrently)
import           Control.Exception (displayException)
import           Control.Lens
import           Control.Monad
import           Data.Blob
import           Data.Foldable
import           Data.Int
import           Data.List
import qualified Data.Text as Text
import           Data.Traversable
import           System.FilePath.Glob
import           System.Path ((</>))
import qualified System.Path as Path
import qualified System.Path.Directory as Path
import qualified System.Process as Process
import qualified System.Path.Fixture as Fixture
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

import Data.Flag
import Proto.Semantic as P hiding (Blob)
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
  , le "python" "**/*.py" pythonFileSkips mempty
  , le "ruby" "**/*.rb" rubySkips mempty
  , le "typescript" "**/*.[jt]s" typescriptSkips mempty
  -- , le "typescript" "**/*.[jt]sx" tsxSkips mempty
  ]

goFileSkips :: [Path.RelFile]
goFileSkips = Path.relPath <$>
  [
  -- Super slow
    "go/src/vendor/golang_org/x/text/unicode/norm/tables.go"
  , "go/src/vendor/golang_org/x/text/unicode/bidi/tables.go"
  , "go/src/vendor/golang_org/x/net/idna/tables.go"
  , "go/src/cmd/vendor/golang.org/x/arch/x86/x86asm/tables.go"
  , "moby/vendor/golang.org/x/text/unicode/norm/tables9.0.0.go"
  , "moby/vendor/golang.org/x/text/unicode/norm/tables10.0.0.go"

  -- Parser timeouts
  , "moby/vendor/github.com/ugorji/go/codec/fast-path.generated.go"

  -- Parse errors
  , "go/src/math/big/arith.go" -- Unhandled identifier character: 'ŝ'
  , "go/src/cmd/go/testdata/src/badpkg/x.go"
  , "go/src/cmd/go/testdata/src/notest/hello.go"
  , "go/src/cmd/vet/testdata/deadcode.go"
  , "go/src/cmd/vet/testdata/testingpkg/tests_test.go"
  , "moby/vendor/github.com/beorn7/perks/quantile/stream.go" -- Unhandled identifier character: 'ƒ'

  ]

goDirSkips :: [Path.RelDir]
goDirSkips = Path.relDir <$>
  [ "go/src/cmd/compile/internal/ssa"
  , "go/test/fixedbugs"
  , "go/test/syntax"
  , "go/test/method4.dir"
  , "go/test"
  ]

pythonFileSkips :: [Path.RelFile]
pythonFileSkips = []

rubySkips :: [Path.RelFile]
rubySkips = Path.relFile <$>
  [
  -- Doesn't parse b/c of issue with r<<i
    "ruby_spec/core/enumerable/shared/inject.rb"
  -- Doesn't parse
  , "ruby_spec/language/string_spec.rb"
  , "ruby_spec/language/fixtures/freeze_magic_comment_required_diff_enc.rb"
  , "ruby_spec/command_line/fixtures/freeze_flag_required_diff_enc.rb"
  , "ruby_spec/command_line/fixtures/bad_syntax.rb"

  -- Can't detect method calls inside heredoc bodies with precise ASTs
  , "ruby_spec/core/argf/readpartial_spec.rb"
  , "ruby_spec/core/process/exec_spec.rb"

  -- These are known differences between precise and a la carte (usually precise is producing better data) that we aren't going to fix.
  , "ruby_spec/language/def_spec.rb"
  , "ruby_spec/language/block_spec.rb"
  , "ruby_spec/language/method_spec.rb"
  , "ruby_spec/language/lambda_spec.rb"
  ]

tsxSkips :: [Path.RelFile]
tsxSkips = Path.relFile <$>
  [
  ]

typescriptSkips :: [Path.RelFile]
typescriptSkips = Path.relFile <$>
  [ "npm/node_modules/slide/lib/async-map-ordered.js"
  , "npm/node_modules/request/node_modules/har-validator/node_modules/ajv/dist/regenerator.min.js"
  ]



buildExamples :: Fixture.HasFixture => TaskSession -> LanguageExample -> Path.AbsRelDir -> IO Tasty.TestTree
buildExamples session lang tsDir = do
  let fileSkips = fmap (tsDir </>) (languageSkips lang)
      dirSkips  = fmap (tsDir </>) (languageDirSkips lang)



  files <- globDir1 (compile (languageExtension lang)) (Path.toString tsDir)
  when (null files)
    (fail ("Nothing in dir " <> Path.toString tsDir))

  let paths = filter (\x -> Path.takeDirectory x `notElem` dirSkips) . filter (`notElem` fileSkips) $ Path.absRel <$> files
  trees <- for paths $ \file -> do
    pure . HUnit.testCase (Path.toString file) $ do
      precise <- runTask session (runParse (parseSymbolsFilePath file))
      assertOK "precise" precise
  pure (Tasty.testGroup (languageName lang) trees)

  where
    assertOK msg = either (\e -> HUnit.assertFailure (msg <> " failed to parse" <> show e)) (refuteErrors msg)
    refuteErrors msg a = case toList (a^.files) of
      [x] | (e:_) <- toList (x^.errors) -> HUnit.assertFailure (msg <> " parse errors " <> show e)
      _                                 -> pure ()

data SortableSymbol = SortableSymbol Text.Text Int32 Int32 Int32 Int32
  deriving (Eq, Show, Ord)


testOptions :: Config.Options
testOptions = defaultOptions
  { optionsFailOnWarning = flag FailOnWarning True
  , optionsLogLevel = Nothing
  }

main :: IO ()
-- main = putStrLn "nothing"
main = withOptions testOptions $ \ config logger statter -> do
  -- void $ Process.system "script/clone-example-repos"

#if BAZEL_BUILD
  rf <- Fixture.create
  let ?runfiles = rf
  let ?project = Path.relDir "semantic"
#endif

  let session = TaskSession config "-" False logger statter

  allTests <- forConcurrently examples $ \lang@LanguageExample{..} -> do
    let tsDir = Fixture.absRelDir ".."
    buildExamples session lang tsDir

  Tasty.defaultMain $ Tasty.testGroup "parse-examples" allTests

parseSymbolsFilePath ::
  ( Has (Error SomeException) sig m
  , Has Parse sig m
  , Has Files sig m
  )
  => Path.AbsRelFile
  -> m ParseTreeSymbolResponse
parseSymbolsFilePath path = readBlob (File.fromPath path) >>= parseSymbols . pure @[]
