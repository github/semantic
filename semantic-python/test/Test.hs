{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE FlexibleInstances, TypeApplications, RecordWildCards, FlexibleContexts, TypeOperators, OverloadedStrings #-}

module Main (main) where

import Prelude hiding (fail)
import           Control.Effect
import           Control.Effect.Fresh
import           Control.Effect.Fail
import           Control.Effect.Reader
import Data.Function
import           Control.Monad hiding (fail)
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy
import           Data.Core
import           Data.File
import           Data.Foldable
import           Data.Loc
import           Data.List (sort, isInfixOf)
import           Data.Name
import           Data.Term
import           GHC.Stack
import qualified Language.Python.Core as Py
import           System.Directory
import           System.FilePath
import qualified TreeSitter.Python as TSP
import qualified TreeSitter.Python.AST as TSP
import qualified TreeSitter.Unmarshal as TS
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Analysis.Eval as Eval
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Streaming.Process
import qualified Data.ByteString.Streaming.Char8 as ByteStream
import System.Exit
import Control.Monad.Catch
import Data.Core.Pretty
import Data.Maybe

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

import qualified Directive
import ScopeDump

instance MonadFail (Either String) where fail = Left

assertTranslationSucceeds :: HasCallStack => FilePath -> HUnit.Assertion
assertTranslationSucceeds fp = withFrozenCallStack $ do
  let skipThisTest = "fails" `isInfixOf` fp || "disabled" `isInfixOf` fp
  unless skipThisTest $ do
    fileContents <- ByteString.readFile ("semantic-python/test/fixtures" </> fp)
    let first = ByteString.takeWhile (/= '\n') fileContents
    directive <- case Directive.parseDirective first of
      Right dir -> pure dir
      Left err -> HUnit.assertFailure ("Directive parsing error: " <> err)


    result <- TS.parseByteString TSP.tree_sitter_python fileContents
    core <- case fmap (Py.compile @TSP.Module @_ @(Term Core)) result of
      Right (Right item) -> pure item
      Right (Left err)   -> HUnit.assertFailure ("Compilation failed: " <> err)
      Left err           -> HUnit.assertFailure ("Parsing failed: " <> err)

    dump <- case runScopeDump _ of
      Left e  -> HUnit.assertFailure ("Couldn't run scope dumping mechanism; this shouldn't happen (" <> e <> ")")
      Right d -> pure $ Aeson.encodePretty d

    let jqPipeline = Streaming.Process.streamInput (Directive.toProcess directive) (ByteStream.fromLazy dump)
        errorMsg = "jq(1) returned non-zero exit code"
        dirMsg    = "jq expression: " <> show directive
        jsonMsg   = "JSON value: " <> ByteString.Lazy.unpack dump
        treeMsg   = "Core expr: " <> showCore core

    catch jqPipeline $ \err -> do
      HUnit.assertFailure (lines [errorMsg, dirMsg, jsonMsg, treeMsg])


milestoneFixtures :: Tasty.TestTree
milestoneFixtures = HUnit.testCaseSteps "Bootstrapping" $ \step -> do
  files <- liftIO (listDirectory "semantic-python/test/fixtures")
  let firstGroup = sort $ filter ((== '1') . head) files
  for_ firstGroup $ \file -> do
    step file
    assertTranslationSucceeds file


tests :: Tasty.TestTree
tests = Tasty.testGroup "Fixtures"
  [ milestoneFixtures
  ]

main :: IO ()
main = do
  jq <- findExecutable "jq"
  when (isNothing jq) (die "Error: jq(1) not found in $PATH.")
  Tasty.defaultMain tests
