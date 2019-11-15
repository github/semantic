{-# LANGUAGE ScopedTypeVariables, TypeOperators, OverloadedStrings, OverloadedLists #-}

module Graph
  ( testTree
  , parseEither
  ) where

import           Analysis.File
import           Analysis.ScopeGraph
import           Control.Effect.Carrier
import           Core.Core
import qualified Core.Eval as Eval
import           Core.Name
import           Core.Parser as Parse
import qualified Data.Map as Map
import           Data.Semilattice.Lower
import           Lens.Micro
import           Source.Span
import           Syntax.Term
import qualified System.Path as Path
import qualified System.Path.IO as Path (readFile)
import           Text.Pretty.Simple
import qualified Text.Trifecta as Trifecta

import qualified Test.Tasty as Tasty
import           Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit as HUnit

parseEither :: Trifecta.Parser a -> String -> Either String a
parseEither p = Trifecta.foldResult (Left . show . Trifecta._errDoc) Right . Trifecta.parseString (p <* Trifecta.eof) mempty

testSimple :: Tasty.TestTree
testSimple = HUnit.testCase "simple.score" $ do
  let p = Path.absRel "semantic-core/test/fixtures/simple.score"
  contents <- Path.readFile p
  case parseEither Parse.core contents of
    Left m  -> HUnit.assertFailure ("Couldn't parse simple.score: " <> m)
    Right (x :: Term (Ann Span :+: Core) Name) -> do
      (_heap, [File { fileBody = Right (ScopeGraph res) }]) <- pure (scopeGraph Eval.eval [File p lowerBound x])
      [(decl, [ref])] <- pure (Map.toList res)
      declSymbol decl @?= "func"
      ref^.span_.start_ @?= Pos 4 2
      ref^.span_.end_   @?= Pos 4 7

testConditional :: Tasty.TestTree
testConditional = HUnit.testCase "conditional.score" $ do
  let p = Path.absRel "semantic-core/test/fixtures/conditional.score"
  contents <- Path.readFile p
  case parseEither Parse.core contents of
    Left m  -> HUnit.assertFailure ("Couldn't parse conditional.score: " <> m)
    Right (x :: Term (Ann Span :+: Core) Name) -> do
      (heap, [File { fileBody = Right (ScopeGraph res) }]) <- pure (scopeGraph Eval.eval [File p lowerBound x])
      [(merle, [refm]), (taako, [reft])] <- pure (Map.toList res)
      refm^.span_.start_ @?= Pos 6 4
      reft^.span_.start_ @?= Pos 7 9

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "Core scope graphing"
  [ testSimple
  , testConditional
  ]
