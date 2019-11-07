{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}
module Diffing.Algorithm.RWS.Spec (spec) where

import Data.Bifunctor
import Data.Diff
import Data.Functor.Listable (ListableSyntax)
import Data.Sum
import qualified Data.Syntax as Syntax
import Data.Term
import Diffing.Algorithm (comparableTerms)
import Diffing.Interpreter (stripDiff)
import Diffing.Algorithm.RWS
import Diffing.Interpreter.Spec (afterTerm, beforeTerm)
import Test.Hspec.LeanCheck
import SpecHelpers

spec :: Spec
spec = do
  let positively = succ . abs
  describe "pqGramDecorator" $ do
    prop "produces grams with stems of the specified length" $
      \ (term, p, q) -> pqGramDecorator (positively p) (positively q) (term :: Term ListableSyntax ()) `shouldSatisfy` all ((== positively p) . length . stem . fst)

    prop "produces grams with bases of the specified width" $
      \ (term, p, q) -> pqGramDecorator (positively p) (positively q) (term :: Term ListableSyntax ()) `shouldSatisfy` all ((== positively q) . length . base . fst)

  describe "rws" $ do
    prop "produces correct diffs" $
      \ (as, bs) -> let tas = decorate <$> (as :: [Term ListableSyntax ()])
                        tbs = decorate <$> (bs :: [Term ListableSyntax ()])
                        wrap = termIn emptyAnnotation . inject
                        diff = merge (emptyAnnotation, emptyAnnotation) (inject (stripDiff . diffEdit <$> rws comparableTerms (equalTerms comparableTerms) tas tbs)) in
        (beforeTerm diff, afterTerm diff) `shouldBe` (Just (wrap (stripTerm <$> tas)), Just (wrap (stripTerm <$> tbs)))

    it "produces unbiased insertions within branches" $
      let (a, b) = (decorate (termIn emptyAnnotation (inject [ termIn emptyAnnotation (inject (Syntax.Identifier "a")) ])), decorate (termIn emptyAnnotation (inject [ termIn emptyAnnotation (inject (Syntax.Identifier "b")) ]))) in
      fmap (bimap stripTerm stripTerm) (rws comparableTerms (equalTerms comparableTerms) [ b ] [ a, b ]) `shouldBe` fmap (bimap stripTerm stripTerm) [ Insert a, Compare b b ]

  where decorate = defaultFeatureVectorDecorator

        diffEdit = edit deleting inserting comparing

stripTerm :: Functor f => Term f (FeatureVector, ()) -> Term f ()
stripTerm = fmap snd

emptyAnnotation :: ()
emptyAnnotation = ()
