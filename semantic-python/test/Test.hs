{-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving, TypeOperators, DerivingStrategies #-}

module Main (main) where

import qualified Analysis.Eval as Eval
import           Analysis.FlowInsensitive
import           Control.Effect
import           Control.Effect.Fail
import           Control.Effect.Fresh
import           Control.Effect.Reader
import           Control.Monad hiding (fail)
import           Control.Monad.Catch
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy
import qualified Data.ByteString.Streaming.Char8 as ByteStream
import           Data.Core
import           Data.Core.Pretty
import           Data.File
import           Data.Foldable
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List (isInfixOf, sort)
import           Data.Loc
import           Data.Maybe
import           Data.Name
import           Data.Term
import           GHC.Stack
import qualified Language.Python.Core as Py
import           Prelude hiding (fail)
import           Streaming
import qualified Streaming.Process
import           System.Directory
import           System.Exit
import           System.FilePath
import qualified TreeSitter.Python as TSP
import qualified TreeSitter.Python.AST as TSP
import qualified TreeSitter.Unmarshal as TS
import qualified Data.Map as Map

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

import qualified Directive
import           Analysis.ScopeGraph
import Instances ()

dumpScopeGraph :: Heap Name ScopeGraph -> ScopeGraph -> Aeson.Value
dumpScopeGraph h sg = Aeson.object $
  [ "scope" Aeson..= h
  , "heap"  Aeson..= sg
  ]


assertJQExpressionSucceeds :: Directive.Directive -> Term (Ann :+: Core) Name -> HUnit.Assertion
assertJQExpressionSucceeds directive core = do
  bod <- case scopeGraph Eval.eval [File interactive core] of
    (heap, [File _ (Right bod)]) -> pure $ dumpScopeGraph heap bod
    other -> HUnit.assertFailure "Couldn't run scope dumping mechanism; this shouldn't happen"

  let ignore = ByteStream.effects . hoist ByteStream.effects
      sgJSON = ByteStream.fromLazy $ Aeson.encode bod
      jqPipeline = Streaming.Process.withStreamingProcess (Directive.toProcess directive) sgJSON ignore
      errorMsg = "jq(1) returned non-zero exit code"
      dirMsg    = "jq expression: " <> show directive
      jsonMsg   = "JSON value: " <> ByteString.Lazy.unpack (Aeson.encodePretty bod)
      treeMsg   = "Core expr: " <> showCore (stripAnnotations core)

  catch @_ @Streaming.Process.ProcessExitedUnsuccessfully jqPipeline $ \err -> do
    HUnit.assertFailure (unlines [errorMsg, dirMsg, jsonMsg, treeMsg, show err])


assertTranslationSucceeds :: HasCallStack => FilePath -> HUnit.Assertion
assertTranslationSucceeds fp = withFrozenCallStack $ do
  let skipThisTest = "fails" `isInfixOf` fp || "disabled" `isInfixOf` fp
  unless skipThisTest $ do
    fileContents <- ByteString.readFile ("semantic-python/test/fixtures" </> fp)
    let first = ByteString.takeWhile (/= '\n') fileContents
    directive <- case Directive.parseDirective first of
      Right dir -> pure dir
      Left err  -> HUnit.assertFailure ("Directive parsing error: " <> err)

    result <- TS.parseByteString TSP.tree_sitter_python fileContents
    let coreResult = fmap (Py.compile @TSP.Module @_ @(Term (Ann :+: Core))) result
    case coreResult of
      Right (Left _) | directive == Directive.Fails -> pure ()
      Right (Right item) -> assertJQExpressionSucceeds directive item
      Right (Left err)   -> HUnit.assertFailure ("Compilation failed: " <> err)
      Left err           -> HUnit.assertFailure ("Parsing failed: " <> err)


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
