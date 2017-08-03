{-# LANGUAGE DataKinds #-}
module Data.RandomWalkSimilarity.Spec where

import Category
import Control.Comonad.Trans.Cofree (headF)
import Control.Monad.Free (wrap)
import Data.Array.IArray
import Data.Bifunctor
import Data.Functor.Listable
import Data.Record
import Data.These
import Diff
import Info
import Patch
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
      \ (term, p, q) -> pqGramDecorator (rhead . headF) (positively p) (positively q) (unListableF term :: SyntaxTerm '[Category]) `shouldSatisfy` all ((== positively p) . length . stem . rhead)

    prop "produces grams with bases of the specified width" $
      \ (term, p, q) -> pqGramDecorator (rhead . headF) (positively p) (positively q) (unListableF term :: SyntaxTerm '[Category]) `shouldSatisfy` all ((== positively q) . length . base . rhead)

  describe "featureVectorDecorator" $ do
    prop "produces a vector of the specified dimension" $
      \ (term, p, q, d) -> featureVectorDecorator (rhead . headF) (positively p) (positively q) (positively d) (unListableF term :: SyntaxTerm '[Category]) `shouldSatisfy` all ((== (0, abs d)) . bounds . rhead)

  describe "rws" $ do
    prop "produces correct diffs" $
      \ (as, bs) -> let tas = decorate <$> (unListableF <$> as :: [SyntaxTerm '[Category]])
                        tbs = decorate <$> (unListableF <$> bs :: [SyntaxTerm '[Category]])
                        root = cofree . ((Program :. Nil) :<) . Indexed
                        diff = wrap (pure (Program :. Nil) :< Indexed (stripDiff . diffThese <$> rws editDistance canCompare tas tbs)) in
        (beforeTerm diff, afterTerm diff) `shouldBe` (Just (root (stripTerm <$> tas)), Just (root (stripTerm <$> tbs)))

    it "produces unbiased insertions within branches" $
      let (a, b) = (decorate (cofree ((StringLiteral :. Nil) :< Indexed [ cofree ((StringLiteral :. Nil) :< Leaf "a") ])), decorate (cofree ((StringLiteral :. Nil) :< Indexed [ cofree ((StringLiteral :. Nil) :< Leaf "b") ]))) in
      fmap (bimap stripTerm stripTerm) (rws editDistance canCompare [ b ] [ a, b ]) `shouldBe` fmap (bimap stripTerm stripTerm) [ That a, These b b ]

  where canCompare a b = headF a == headF b

        decorate :: SyntaxTerm '[Category] -> SyntaxTerm '[FeatureVector, Category]
        decorate = defaultFeatureVectorDecorator (category . headF)

        diffThese = these deleting inserting replacing

        editDistance = these (const 1) (const 1) (const (const 0))
