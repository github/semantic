{-# LANGUAGE DataKinds #-}
module Data.RandomWalkSimilarity.Spec where

import Data.Array.IArray
import Data.Bifunctor
import Data.Functor.Listable (ListableSyntax)
import Data.Record
import qualified Data.Syntax as Syntax
import Data.These
import Data.Union
import Decorators
import Diff
import Interpreter
import RWS
import Term
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = parallel $ do
  let positively = succ . abs
  describe "pqGramDecorator" $ do
    prop "produces grams with stems of the specified length" $
      \ (term, p, q) -> pqGramDecorator constructorNameAndConstantFields (positively p) (positively q) (term :: Term ListableSyntax (Record '[])) `shouldSatisfy` all ((== positively p) . length . stem . rhead)

    prop "produces grams with bases of the specified width" $
      \ (term, p, q) -> pqGramDecorator constructorNameAndConstantFields (positively p) (positively q) (term :: Term ListableSyntax (Record '[])) `shouldSatisfy` all ((== positively q) . length . base . rhead)

  describe "featureVectorDecorator" $ do
    prop "produces a vector of the specified dimension" $
      \ term p q d -> featureVectorDecorator constructorNameAndConstantFields (positively p) (positively q) (positively d) (term :: Term ListableSyntax (Record '[])) `shouldSatisfy` all ((== (0, abs d)) . bounds . unFV . rhead)

  describe "rws" $ do
    prop "produces correct diffs" $
      \ (as, bs) -> let tas = decorate <$> (as :: [Term ListableSyntax (Record '[])])
                        tbs = decorate <$> (bs :: [Term ListableSyntax (Record '[])])
                        wrap = termIn Nil . inj
                        diff = merge (Nil, Nil) (inj (stripDiff . diffThese <$> rws comparableByConstructor (equalTerms comparableByConstructor) tas tbs)) in
        (beforeTerm diff, afterTerm diff) `shouldBe` (Just (wrap (stripTerm <$> tas)), Just (wrap (stripTerm <$> tbs)))

    it "produces unbiased insertions within branches" $
      let (a, b) = (decorate (termIn Nil (inj [ termIn Nil (inj (Syntax.Identifier "a")) ])), decorate (termIn Nil (inj [ termIn Nil (inj (Syntax.Identifier "b")) ]))) in
      fmap (bimap stripTerm stripTerm) (rws comparableByConstructor (equalTerms comparableByConstructor) [ b ] [ a, b ]) `shouldBe` fmap (bimap stripTerm stripTerm) [ That a, These b b ]

  where decorate = defaultFeatureVectorDecorator constructorNameAndConstantFields

        diffThese = these deleting inserting replacing
