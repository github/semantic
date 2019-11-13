{-# LANGUAGE ScopedTypeVariables #-}

module Graph
  ( testTree
  , parseEither
  ) where

import qualified Analysis.ScopeGraph as ScopeGraph
import           Core.Core
import           Core.Name
import           Core.Parser as Parse
import           Syntax.Term
import qualified Text.Trifecta as Trifecta

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

parseEither :: Trifecta.Parser a -> String -> Either String a
parseEither p = Trifecta.foldResult (Left . show . Trifecta._errDoc) Right . Trifecta.parseString (p <* Trifecta.eof) mempty

testSimpleScopeGraph :: Tasty.TestTree
testSimpleScopeGraph = HUnit.testCase "simple.score" $ do
  contents <- readFile "test/fixtures/simple.score"
  (_parsed :: Term Core Name) <- case parseEither Parse.core contents of
    Right x -> pure x
    Left m  -> HUnit.assertFailure ("Couldn't parse simple.score: " <> m)
  pure ()

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "Core scope graphing"
  [ testSimpleScopeGraph
  ]
