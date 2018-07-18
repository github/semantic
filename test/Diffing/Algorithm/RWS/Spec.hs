{-# LANGUAGE DataKinds #-}
module Diffing.Algorithm.RWS.Spec where

import Analysis.Decorator
import Data.Bifunctor
import Data.Diff
import Data.Functor.Listable (ListableSyntax)
import Data.Record
import Data.Sum
import qualified Data.Syntax as Syntax
import Data.Term
import Data.These
import Diffing.Algorithm
import Diffing.Algorithm.RWS
import Test.Hspec.LeanCheck
import SpecHelpers

spec :: Spec
spec = parallel $ do
  let positively = succ . abs
  describe "pqGramDecorator" $ do
    prop "produces grams with stems of the specified length" $
      \ (term, p, q) -> pqGramDecorator (positively p) (positively q) (term :: Term ListableSyntax (Record '[])) `shouldSatisfy` all ((== positively p) . length . stem . rhead)

    prop "produces grams with bases of the specified width" $
      \ (term, p, q) -> pqGramDecorator (positively p) (positively q) (term :: Term ListableSyntax (Record '[])) `shouldSatisfy` all ((== positively q) . length . base . rhead)

  describe "rws" $ do
    prop "produces correct diffs" $
      \ (as, bs) -> let tas = decorate <$> (as :: [Term ListableSyntax (Record '[])])
                        tbs = decorate <$> (bs :: [Term ListableSyntax (Record '[])])
                        wrap = termIn Nil . inject
                        diff = merge (Nil, Nil) (inject (stripDiff . diffThese <$> rws comparableTerms (equalTerms comparableTerms) tas tbs)) in
        (beforeTerm diff, afterTerm diff) `shouldBe` (Just (wrap (stripTerm <$> tas)), Just (wrap (stripTerm <$> tbs)))

    it "produces unbiased insertions within branches" $
      let (a, b) = (decorate (termIn Nil (inject [ termIn Nil (inject (Syntax.Identifier "a")) ])), decorate (termIn Nil (inject [ termIn Nil (inject (Syntax.Identifier "b")) ]))) in
      fmap (bimap stripTerm stripTerm) (rws comparableTerms (equalTerms comparableTerms) [ b ] [ a, b ]) `shouldBe` fmap (bimap stripTerm stripTerm) [ That a, These b b ]

  where decorate = defaultFeatureVectorDecorator

        diffThese = these deleting inserting replacing
