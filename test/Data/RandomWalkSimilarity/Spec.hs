{-# LANGUAGE DataKinds #-}
module Data.RandomWalkSimilarity.Spec where

import Category
import Data.Array.IArray
import Data.Bifunctor
import Data.Functor.Listable ()
import Data.Record
import Data.These
import Diff
import Info
import RWS
import Syntax
import Term
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = parallel $ do
  let positively = succ . abs
  describe "pqGramDecorator" $ do
    prop "produces grams with stems of the specified length" $
      \ (term, p, q) -> pqGramDecorator (rhead . termAnnotation) (positively p) (positively q) (term :: SyntaxTerm '[Category]) `shouldSatisfy` all ((== positively p) . length . stem . rhead)

    prop "produces grams with bases of the specified width" $
      \ (term, p, q) -> pqGramDecorator (rhead . termAnnotation) (positively p) (positively q) (term :: SyntaxTerm '[Category]) `shouldSatisfy` all ((== positively q) . length . base . rhead)

  describe "featureVectorDecorator" $ do
    prop "produces a vector of the specified dimension" $
      \ (term, p, q, d) -> featureVectorDecorator (rhead . termAnnotation) (positively p) (positively q) (positively d) (term :: SyntaxTerm '[Category]) `shouldSatisfy` all ((== (0, abs d)) . bounds . rhead)

  describe "rws" $ do
    prop "produces correct diffs" $
      \ (as, bs) -> let tas = decorate <$> (as :: [SyntaxTerm '[Category]])
                        tbs = decorate <$> (bs :: [SyntaxTerm '[Category]])
                        root = termIn (Program :. Nil) . Indexed
                        diff = copy (pure (Program :. Nil)) (Indexed (stripDiff . diffThese <$> rws editDistance canCompare tas tbs)) in
        (beforeTerm diff, afterTerm diff) `shouldBe` (Just (root (stripTerm <$> tas)), Just (root (stripTerm <$> tbs)))

    it "produces unbiased insertions within branches" $
      let (a, b) = (decorate (Term ((StringLiteral :. Nil) `In` Indexed [ Term ((StringLiteral :. Nil) `In` Leaf "a") ])), decorate (Term ((StringLiteral :. Nil) `In` Indexed [ Term ((StringLiteral :. Nil) `In` Leaf "b") ]))) in
      fmap (bimap stripTerm stripTerm) (rws editDistance canCompare [ b ] [ a, b ]) `shouldBe` fmap (bimap stripTerm stripTerm) [ That a, These b b ]

  where canCompare a b = termAnnotation a == termAnnotation b

        decorate :: SyntaxTerm '[Category] -> SyntaxTerm '[FeatureVector, Category]
        decorate = defaultFeatureVectorDecorator (category . termAnnotation)

        diffThese = these deleting inserting replacing

        editDistance = these (const 1) (const 1) (const (const 0))
