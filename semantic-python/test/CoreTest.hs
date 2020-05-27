{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main (main) where

import           Analysis.Concrete (Concrete)
import qualified Analysis.Concrete as Concrete
import           Analysis.File
import           Control.Algebra
import           Control.Carrier.Fail.Either
import           Control.Carrier.Reader
import           Control.Monad hiding (fail)
import           Control.Monad.IO.Class
-- import           Core.Core
-- import qualified Core.Eval as Eval
-- import           Core.Name
-- import qualified Core.Parser
-- import           Core.Pretty
import qualified Data.ByteString.Char8 as ByteString
import           Data.Foldable
import           Data.Function
import qualified Data.IntMap as IntMap
import           Data.List (sort)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (Text)
import           GHC.Stack
-- import qualified Language.Python.Core as Py
import           Language.Python.Failure
import           Prelude hiding (fail)
import           Source.Span
import           Syntax.Term
import           Syntax.Var (closed)
import           System.Directory
import           System.Exit
import           System.Path ((</>))
import qualified System.Path as Path
import qualified System.Path.Directory as Path
import qualified Text.Trifecta as Trifecta
import qualified Language.Python.Grammar as TSP
import qualified AST.Unmarshal as TS

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

-- import qualified Directive
import           Instances ()

parsePrelude :: IO (Term (Ann Span :+: Core) Name)
parsePrelude = do
  preludesrc <- ByteString.readFile "semantic-python/src/Prelude.score"
  let ePrelude = Trifecta.parseByteString (Core.Parser.core <* Trifecta.eof) mempty preludesrc
  case Trifecta.foldResult (Left . show) Right ePrelude of
    Right r -> pure r
    Left s  -> HUnit.assertFailure ("Couldn't parse prelude: " <> s)

-- handles CHECK-RESULT directives
assertEvaluatesTo :: Term (Ann Span :+: Core) Name -> Text -> Concrete (Term (Ann Span :+: Core)) -> HUnit.Assertion
assertEvaluatesTo core k val = do
  prelude <- parsePrelude
  let withPrelude = (named' "__semantic_prelude" :<- prelude) >>>= core
  allTogether <- maybe (HUnit.assertFailure ("Can’t evaluate open term: " <> showCore (stripAnnotations withPrelude))) pure (closed withPrelude)
  let filius = [File (Path.absRel "<interactive>") (Span (Pos 1 1) (Pos 1 1)) allTogether]

  (heap, env) <- case Concrete.concrete Eval.eval filius of
    (heap, [File _ _ (Right (Concrete.Record env))]) ->
      pure (heap, env)
    (_, [File _ _ (Left (_, span, err))]) ->
      HUnit.assertFailure ("Failed evaluation (" <> show span <> "): " <> err)
    (_, files) ->
      HUnit.assertFailure ("Unexpected number of files: " <> show (length files))

  let found = Map.lookup (name k) env >>= flip IntMap.lookup heap
  found HUnit.@?= Just val
{-# HLINT ignore assertEvaluatesTo #-}

-- handles CHECK-TREE directives
assertTreeEqual :: Term Core Name -> Term Core Name -> HUnit.Assertion
assertTreeEqual t item = HUnit.assertEqual ("got (pretty)" <> showCore item) t item


checkPythonFile :: HasCallStack => Path.RelFile -> Tasty.TestTree
checkPythonFile fp = HUnit.testCaseSteps (Path.toString fp) $ \step -> withFrozenCallStack $ do
  -- Extract the directives and the core associated with the provided file
  let fullPath  = Path.relDir "semantic-python/test/fixtures" </> fp
  directives <- Directive.readDirectivesFromFile fullPath
  result <- ByteString.readFile (Path.toString fullPath) >>= TS.parseByteString TSP.tree_sitter_python

  -- Run the compiler
  let coreResult = Control.Algebra.run
                   . runFail
                   . eliminateFailures
                   . Control.Algebra.run
                   . runReader @Py.Bindings mempty
                   . Py.toplevelCompile @(Failure :+: Ann Span :+: Core) @(Term _)
                   <$> result

  -- Dispatch based on the result-directive pair
  for_ directives $ \directive -> do
    step (Directive.describe directive)
    case (coreResult, directive) of
      (Right (Left _), Directive.Fails)          -> pure ()
      (Left err, _)                              -> HUnit.assertFailure ("Parsing failed: " <> err)
      (Right (Left err), _)                      -> HUnit.assertFailure ("Compilation failed: " <> err)
      (Right (Right _), Directive.Fails)         -> HUnit.assertFailure "Expected translation to fail"
      (Right (Right item), Directive.Result k v) -> assertEvaluatesTo item k v
      (Right (Right item), Directive.Tree t)     -> assertTreeEqual (stripAnnotations item) t

milestoneFixtures :: IO Tasty.TestTree
milestoneFixtures = buildTests <$> readFiles
  where
    readFiles  = liftIO . Path.filesInDir . Path.relDir $ "semantic-python/test/fixtures"
    buildTests = Tasty.testGroup "Python" . fmap checkPythonFile . sort . filter (Path.hasExtension ".py")

main :: IO ()
main = do
  jq <- findExecutable "jq"
  when (isNothing jq) (die "Error: jq(1) not found in $PATH.")
  milestoneFixtures >>= Tasty.defaultMain
