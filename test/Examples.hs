{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
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
import           Data.Language (LanguageMode (..), PerLanguageModes (..), aLaCarteLanguageModes, preciseLanguageModes)
import           Data.List
import qualified Data.Text as Text
import           Data.Traversable
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
  , le "python" "**/*.py" pythonFileSkips mempty
  , le "ruby" "**/*.rb" rubySkips mempty
  , le "typescript" "**/*.[jt]s" typescriptSkips mempty
  , le "typescript" "**/*.[jt]sx" tsxSkips mempty
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

  -- Assignment timeouts
  , "go/src/cmd/compile/internal/gc/constFold_test.go"
  , "go/src/cmd/compile/internal/gc/testdata/arithConst.go"
  , "moby/vendor/github.com/docker/swarmkit/api/types.pb.go"
  , "moby/vendor/github.com/docker/swarmkit/api/control.pb.go"

  -- Parser timeouts
  , "moby/vendor/github.com/ugorji/go/codec/fast-path.generated.go"

  -- Parse errors
  , "go/src/math/big/arith.go" -- Unhandled identifier character: 'ŝ'
  , "go/src/cmd/go/testdata/src/badpkg/x.go"
  , "go/src/cmd/go/testdata/src/notest/hello.go"
  , "go/src/cmd/vet/testdata/deadcode.go"
  , "go/src/cmd/vet/testdata/testingpkg/tests_test.go"
  , "moby/vendor/github.com/beorn7/perks/quantile/stream.go" -- Unhandled identifier character: 'ƒ'

  -- A la carte struggles on these
  , "src/cmd/go/testdata/src/notest/hello.go" -- a la carte chokes on ParseError
  , "go/src/cmd/asm/internal/asm/parse.go" -- a la carte spans are off on line 1124
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
pythonFileSkips = Path.relPath <$>
  [
  -- Assignment doesn't handle f-strings
    "thealgorithms/analysis/compression_analysis/psnr.py"
  , "thealgorithms/maths/greater_common_divisor.py"
  ]

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
  [
  -- Assignment timeouts
    "npm/node_modules/request/node_modules/http-signature/node_modules/sshpk/node_modules/tweetnacl/nacl-fast.js"
  , "npm/node_modules/cli-table2/test/cell-test.js"
  , "npm/node_modules/request/node_modules/har-validator/node_modules/ajv/dist/regenerator.min.js"
  , "npm/node_modules/request/node_modules/har-validator/node_modules/ajv/dist/ajv.bundle.js"
  , "npm/node_modules/request/node_modules/har-validator/node_modules/ajv/dist/ajv.min.js"
  , "npm/node_modules/request/node_modules/har-validator/node_modules/ajv/dist/nodent.min.js"
  , "npm/node_modules/bluebird/js/browser/bluebird.js"
  , "npm/node_modules/bluebird/js/browser/bluebird.min.js"
  , "npm/node_modules/bluebird/js/browser/bluebird.core.js"
  , "npm/node_modules/cli-table2/node_modules/lodash/index.js"
  , "npm/node_modules/cli-table2/node_modules/lodash/index.js"

  -- Parse errors
  , "npm/node_modules/slide/lib/async-map-ordered.js"
  ]

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
      _                                 -> pure ()

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
          -- Check paths
          HUnit.assertEqual "Expected paths to be equal" (x^.path) (y^.path)

          -- Check symbols
          let aLaCarteSymbols = sort . filterALaCarteSymbols (languageName lang) $ toListOf (symbols . traverse . symbol) x
              preciseSymbols = sort . filterALaCarteSymbols (languageName lang) $ toListOf (symbols . traverse . symbol) y
              delta = aLaCarteSymbols \\ preciseSymbols
              invDelta = preciseSymbols \\ aLaCarteSymbols
              msg = "Found in a la carte, but not precise: "
                  <> show delta
                  <> "\n"
                  <> "Found in precise but not a la carte: "
                  <> show invDelta
                  <> "\n"
                  <> "Expected: " <> show aLaCarteSymbols <> "\n"
                  <> "But got:" <> show preciseSymbols
          HUnit.assertBool ("Expected symbols to be equal.\n" <> msg) (null delta)
          HUnit.assertBool ("Expected symbols to be equal.\n" <> msg) (null invDelta)

          -- Check details
          let aLaCarteSymbols = sortOn sSym . filter (okALaCarteSymbol (languageName lang) . view symbol) $ toList (x^.symbols)
              preciseSymbols  = sortOn sSym . filter (okALaCarteSymbol (languageName lang) . view symbol) $ toList (y^.symbols)
          for_ (zip aLaCarteSymbols preciseSymbols) $ \ (left, right) -> do
            let lineNo = ":" <> show (left^.P.span^.start^.line)
                -- lSpan = " [" <> show (startRow left) <> ", " <> show (left^.P.span^.start^.column) <>  "]"
                -- rSpan = " [" <> show (startRow right) <> ", " <> show (right^.P.span^.start^.column) <>  "]"
            HUnit.assertEqual (Text.unpack (x^.path) <> lineNo) (left^.symbol) (right^.symbol)
            HUnit.assertEqual (Text.unpack (x^.path) <> lineNo) (Text.unpack (left^.symbol) <> span left) (Text.unpack (right^.symbol) <> span right)

            -- HUnit.assertEqual (Text.unpack (x^.path) <> lineNo) (left^.line) (right^.line)
            -- HUnit.assertBool (Text.unpack (x^.path) <> lineNo) (Text.isPrefixOf (left^.line) (right^.line))
            -- if left^.kind == "Method"
            --   then HUnit.assertEqual (Text.unpack (x^.path) <> lineNo) (left^.line) (right^.line)
            -- --   -- then HUnit.assertBool (Text.unpack (x^.path) <> lineNo) (Text.isPrefixOf (left^.line) (right^.line))
            --   else pure ()

        _          -> HUnit.assertFailure "Expected 1 file in each response"
      (Left e1, Left e2) -> HUnit.assertFailure ("Unable to parse (both)" <> show (displayException e1) <> show (displayException e2))
      (_, Left e)        -> HUnit.assertFailure ("Unable to parse (precise)" <> show (displayException e))
      (Left e, _)        -> HUnit.assertFailure ("Unable to parse (a la carte)" <> show (displayException e))

    sSym x = SortableSymbol (x^.symbol) (x^.P.span^.start^.line) (x^.P.span^.start^.column) (x^.P.span^.end^.line) (x^.P.span^.end^.column)
    span x = " ["  <> show (x^.P.span^.start^.line) <> ", " <> show (x^.P.span^.start^.column) <>
             " - " <> show (x^.P.span^.end^.line) <> ", " <> show (x^.P.span^.end^.column) <> "]"

data SortableSymbol = SortableSymbol Text.Text Int32 Int32 Int32 Int32
  deriving (Eq, Show, Ord)


okALaCarteSymbol :: String -> Text.Text -> Bool
okALaCarteSymbol "typescript" symbol = symbol `notElem` blacklist
  where
    blacklist = ["require"]
okALaCarteSymbol "ruby" symbol = not (instanceVariable symbol || builtInMethod symbol)
  where
    instanceVariable = Text.isPrefixOf "@"
    builtInMethod x = x `elem` blacklist
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
okALaCarteSymbol _ _ = True

filterALaCarteSymbols :: String -> [Text.Text] -> [Text.Text]
filterALaCarteSymbols lang = filter (okALaCarteSymbol lang)

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

parseSymbolsFilePath ::
  ( Has (Error SomeException) sig m
  , Has Distribute sig m
  , Has Parse sig m
  , Has Files sig m
  )
  => PerLanguageModes
  -> Path.RelFile
  -> m ParseTreeSymbolResponse
parseSymbolsFilePath languageModes path = readBlob (File.fromPath path) >>= runReader languageModes . parseSymbols . pure @[]
