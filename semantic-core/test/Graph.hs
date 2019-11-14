{-# LANGUAGE ScopedTypeVariables #-}

module Graph
  ( testTree
  , parseEither
  ) where

import           Control.Effect.Carrier
import qualified Analysis.ScopeGraph as ScopeGraph
import           Core.Core
import           Core.Name
import           Core.Parser as Parse
import           Syntax.Term
import qualified Text.Trifecta as Trifecta
import           Data.Semilattice.Lower
import qualified Core.Eval as Eval
import           Analysis.File
import qualified System.Path as Path
import qualified System.Path.IO as Path (readFile)

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

parseEither :: Trifecta.Parser a -> String -> Either String a
parseEither p = Trifecta.foldResult (Left . show . Trifecta._errDoc) Right . Trifecta.parseString (p <* Trifecta.eof) mempty

testSimpleScopeGraph :: Tasty.TestTree
testSimpleScopeGraph = HUnit.testCase "simple.score" $ do
  let p = Path.absRel "semantic-core/test/fixtures/simple.score"
  contents <- Path.readFile p
  case parseEither Parse.core contents of
    Left m  -> HUnit.assertFailure ("Couldn't parse simple.score: " <> m)
    Right (x :: Term Core Name) -> do
      (_heap, [_res]) <- pure (ScopeGraph.scopeGraph Eval.eval [File p lowerBound (hoistTerm inj x)])
      pure ()

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "Core scope graphing"
  [ testSimpleScopeGraph
  ]
