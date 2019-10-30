{-# LANGUAGE DeriveAnyClass, DerivingStrategies, GeneralizedNewtypeDeriving, OverloadedStrings, TypeApplications, TypeOperators, ScopedTypeVariables #-}

module Main (main) where

import           Analysis.File
import           Analysis.ScopeGraph
import           Control.Effect
import           Control.Effect.Fail
import           Control.Effect.Reader
import           Control.Monad hiding (fail)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Core.Core
import qualified Core.Parser
import           Core.Pretty
import qualified Core.Eval as Eval
import           Core.Name
import qualified Data.Aeson as Aeson
import           Analysis.Concrete (Concrete)
import qualified Analysis.Concrete as Concrete
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy
import qualified Data.ByteString.Streaming.Char8 as ByteStream
import           Data.Foldable
import           Data.Function
import           Data.List (sort)
import           Data.Maybe
import           GHC.Stack
import qualified Language.Python.Core as Py
import           Prelude hiding (fail)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import           Source.Span
import           Streaming
import qualified Streaming.Prelude as Stream
import qualified Streaming.Process
import           Syntax.Term
import           System.Directory
import           System.Exit
import qualified System.Path as Path
import qualified System.Path.Directory as Path
import           System.Path ((</>))
import           Text.Show.Pretty (ppShow)
import qualified Text.Trifecta as Trifecta
import qualified TreeSitter.Python as TSP
import Data.Text (Text)
import qualified TreeSitter.Unmarshal as TS

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

import qualified Directive
import           Instances ()

parsePrelude :: IO (Term (Ann Span :+: Core) Name)
parsePrelude = do
  preludesrc <- ByteString.readFile "semantic-python/src/Prelude.score"
  let ePrelude = Trifecta.parseByteString (Core.Parser.core <* Trifecta.eof) mempty preludesrc
  case Trifecta.foldResult (Left . show) Right ePrelude of
    Right r -> pure r
    Left s -> HUnit.assertFailure ("Couldn't parse prelude: " <> s)

assertJQExpressionSucceeds :: Show a => Directive.Directive -> a -> Term (Ann Span :+: Core) Name -> HUnit.Assertion
assertJQExpressionSucceeds directive tree core = do
  prelude <- parsePrelude
  let allTogether = (named' "__semantic_prelude" :<- prelude) >>>= core

  bod <- case scopeGraph Eval.eval [File (Path.absRel "<interactive>") (Span (Pos 1 1) (Pos 1 1)) allTogether] of
    (heap, [File _ _ (Right result)]) -> pure $ Aeson.object
      [ "scope" Aeson..= heap
      , "heap"  Aeson..= result
      ]
    other -> HUnit.assertFailure ("Couldn't run scope dumping mechanism: " <> showCore (stripAnnotations allTogether) <> "\n" <> show other)

  let ignore = ByteStream.effects . hoist ByteStream.effects
      sgJSON = ByteStream.fromLazy $ Aeson.encode bod
      jqPipeline = Streaming.Process.withStreamingProcess (Directive.toProcess directive) sgJSON ignore
      errorMsg = "jq(1) returned non-zero exit code"
      dirMsg    = "jq expression: " <> show directive
      jsonMsg   = "JSON value: " <> ByteString.Lazy.unpack (Aeson.encodePretty bod)
      astMsg    = "AST (pretty): " <> ppShow tree
      treeMsg   = "Core expr (pretty): " <> showCore (stripAnnotations core)
      treeMsg'  = "Core expr (Show): " <> ppShow (stripAnnotations core)


  catch @_ @Streaming.Process.ProcessExitedUnsuccessfully jqPipeline $ \err -> do
    HUnit.assertFailure (unlines [errorMsg, dirMsg, jsonMsg, astMsg, treeMsg, treeMsg', show err])

assertEvaluatesTo :: Term (Ann Span :+: Core) Name -> Text -> Directive.Expected -> HUnit.Assertion
assertEvaluatesTo core k val = do
  prelude <- parsePrelude
  let allTogether = (named' "__semantic_prelude" :<- prelude) >>>= core
  let filius = [File (Path.absRel "<interactive>") (Span (Pos 1 1) (Pos 1 1)) allTogether]

  (heap, env) <- case Concrete.concrete Eval.eval filius of
    (heap, [File _ _ (Right (Concrete.Record env))]) -> pure (heap, env)
    other -> error ("SHIT! " <> show other)
  print env
  let found = Map.lookup (Name k) env >>= flip IntMap.lookup heap
  case (found, val) of
    (Just (Concrete.String t), Directive.AString t') -> t HUnit.@?= t'
    (Just Concrete.Unit, Directive.AUnit) -> return ()
    other -> error ("FUCK! " <> show other)



fixtureTestTreeForFile :: HasCallStack => Path.RelFile -> Tasty.TestTree
fixtureTestTreeForFile fp = HUnit.testCaseSteps (Path.toString fp) $ \step -> withFrozenCallStack $ do
  let fullPath  = Path.relDir "semantic-python/test/fixtures" </> fp
      perish s  = liftIO (HUnit.assertFailure ("Directive parsing error: " <> s))
      isComment = (== Just '#') . fmap fst . ByteString.uncons


  -- Slurp the input file, taking lines from the beginning until we
  -- encounter a line that doesn't have a '#'. For each line, parse
  -- a directive out of it, failing if the directive can't be parsed.
  directives <-
    runResourceT
    . Stream.toList_
    . Stream.mapM (either perish pure . Directive.parseDirective)
    . Stream.takeWhile isComment
    . Stream.mapped ByteStream.toStrict
    . ByteStream.lines
    . ByteStream.readFile @(ResourceT IO)
    $ Path.toString fullPath

  result <- ByteString.readFile (Path.toString fullPath) >>= TS.parseByteString TSP.tree_sitter_python
  let coreResult = Control.Effect.run
                   . runFail
                   . runReader @Py.Bindings mempty
                   . Py.toplevelCompile
                   <$> result

  for_ directives $ \directive -> do
    step (Directive.describe directive)
    case (coreResult, directive) of
      (Right (Left _), Directive.Fails)      -> pure ()
      (Left err, _)                          -> HUnit.assertFailure ("Parsing failed: " <> err)
      (Right (Left err), _)                  -> HUnit.assertFailure ("Compilation failed: " <> err)
      (Right (Right _), Directive.Fails)     -> HUnit.assertFailure ("Expected translation to fail")
      (Right (Right item), Directive.Result k v) -> assertEvaluatesTo item k v
      (Right (Right item), Directive.JQ _)   -> assertJQExpressionSucceeds directive result item
      (Right (Right item), Directive.Tree t) -> let msg = "got (pretty): " <> showCore item'
                                                    item' = stripAnnotations item
                                                in HUnit.assertEqual msg t item' where

milestoneFixtures :: IO Tasty.TestTree
milestoneFixtures = do
  files <- liftIO (Path.filesInDir (Path.relDir "semantic-python/test/fixtures"))
  let pythons = sort (filter (Path.hasExtension ".py") files)
  pure $ Tasty.testGroup "Translation" (fmap fixtureTestTreeForFile pythons)

main :: IO ()
main = do
  jq <- findExecutable "jq"
  when (isNothing jq) (die "Error: jq(1) not found in $PATH.")
  milestoneFixtures >>= Tasty.defaultMain
