{-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving, TypeOperators, DerivingStrategies #-}

module Main (main) where

import qualified Analysis.Eval as Eval
import           Analysis.FlowInsensitive
import           Control.Effect
import           Control.Effect.Fail
import           Control.Monad hiding (fail)
import           Control.Monad.Catch
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
import           Data.List (sort)
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
    _other -> HUnit.assertFailure "Couldn't run scope dumping mechanism; this shouldn't happen"

  let ignore = ByteStream.effects . hoist ByteStream.effects
      sgJSON = ByteStream.fromLazy $ Aeson.encode bod
      jqPipeline = Streaming.Process.withStreamingProcess (Directive.toProcess directive) sgJSON ignore
      errorMsg = "jq(1) returned non-zero exit code"
      dirMsg    = "jq expression: " <> show directive
      jsonMsg   = "JSON value: " <> ByteString.Lazy.unpack (Aeson.encodePretty bod)
      treeMsg   = "Core expr: " <> showCore (stripAnnotations core)

  catch @_ @Streaming.Process.ProcessExitedUnsuccessfully jqPipeline $ \err -> do
    HUnit.assertFailure (unlines [errorMsg, dirMsg, jsonMsg, treeMsg, show err])

fixtureTestTreeForFile :: HasCallStack => FilePath -> Tasty.TestTree
fixtureTestTreeForFile fp = HUnit.testCaseSteps fp $ \step -> withFrozenCallStack $ do
  fileContents <- ByteString.readFile ("semantic-python/test/fixtures" </> fp)
  directives <- case Directive.parseDirectives fileContents of
    Right dir -> pure dir
    Left err  -> HUnit.assertFailure ("Directive parsing error: " <> err)

  result <- TS.parseByteString TSP.tree_sitter_python fileContents
  let coreResult = fmap (Control.Effect.run . runFail . Py.compile @TSP.Module @_ @(Term (Ann :+: Core))) result
  for_ directives $ \directive -> do
    step (Directive.describe directive)
    case coreResult of
      Left err -> HUnit.assertFailure ("Parsing failed: " <> err)
      Right (Left _)  | directive == Directive.Fails -> pure ()
      Right (Right _) | directive == Directive.Fails -> HUnit.assertFailure ("Expected translation to fail")
      Right (Right item) -> assertJQExpressionSucceeds directive item
      Right (Left err)   -> HUnit.assertFailure ("Compilation failed: " <> err)


milestoneFixtures :: IO Tasty.TestTree
milestoneFixtures = do
  files <- liftIO (listDirectory "semantic-python/test/fixtures")
  let pythons = sort (filter ("py" `isExtensionOf`) files)
  pure $ Tasty.testGroup "Translation" (fmap fixtureTestTreeForFile pythons)

main :: IO ()
main = do
  jq <- findExecutable "jq"
  when (isNothing jq) (die "Error: jq(1) not found in $PATH.")
  milestoneFixtures >>= Tasty.defaultMain
