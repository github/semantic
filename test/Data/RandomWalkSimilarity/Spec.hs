{-# LANGUAGE DataKinds #-}
module Data.RandomWalkSimilarity.Spec where

import Data.RandomWalkSimilarity
import Data.Record
import Diff
import Info
import Patch
import Prologue
import Syntax
import Term
import Term.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  let positively = succ . abs
  describe "pqGramDecorator" $ do
    prop "produces grams with stems of the specified length" $
      \ (term, p, q) -> pqGramDecorator (rhead . headF) (positively p) (positively q) (toTerm term :: Term Text (Record '[Text])) `shouldSatisfy` all ((== (positively p)) . length . stem . rhead)

    prop "produces grams with bases of the specified width" $
      \ (term, p, q) -> pqGramDecorator (rhead . headF) (positively p) (positively q) (toTerm term :: Term Text (Record '[Text])) `shouldSatisfy` all ((== (positively q)) . length . base . rhead)

  describe "featureVectorDecorator" $ do
    prop "produces a vector of the specified dimension" $
      \ (term, p, q, d) -> featureVectorDecorator (rhead . headF) (positively p) (positively q) (positively d) (toTerm term :: Term Text (Record '[Text])) `shouldSatisfy` all ((== (positively d)) . length . rhead)

  describe "rws" $ do
    let compare a b = if extract a == extract b then Just (pure (Replace a b)) else Nothing
    let toTerm' = featureVectorDecorator (category . headF) 2 2 15 . toTerm
    prop "produces correct diffs" . forAll (scale (`div` 4) arbitrary) $
      \ (as, bs) -> let tas = toTerm' <$> (as :: [ArbitraryTerm Text (Record '[Category])])
                        tbs = toTerm' <$> (bs :: [ArbitraryTerm Text (Record '[Category])])
                        diff = free (Free (pure (pure 0 .: Program .: RNil) :< Indexed (rws compare tas tbs))) in
        (beforeTerm diff, afterTerm diff) `shouldBe` (Just (cofree ((pure 0 .: Program .: RNil) :< Indexed tas)), Just (cofree ((pure 0 .: Program .: RNil) :< Indexed tbs)))
