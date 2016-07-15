module Data.RandomWalkSimilarity.Spec where

import Category
import Data.DList as DList hiding (toList)
import Data.RandomWalkSimilarity
import Diff
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
  describe "pqGrams" $ do
    prop "produces grams with stems of the specified length" . forAll (arbitrary `suchThat` (\ (_, p, q) -> p > 0 && q > 0)) $
      \ (term, p, q) -> pqGrams p q headF (toTerm term :: Term Text Text) `shouldSatisfy` all ((== p) . length . stem)

    prop "produces grams with bases of the specified length" . forAll (arbitrary `suchThat` (\ (_, p, q) -> p > 0 && q > 0)) $
      \ (term, p, q) -> pqGrams p q headF (toTerm term :: Term Text Text) `shouldSatisfy` all ((== q) . length . base)

  describe "featureVector" $ do
    prop "produces a vector of the specified dimension" . forAll (arbitrary `suchThat` ((> 0) . Prologue.snd)) $
      \ (grams, d) -> length (featureVector d (fromList (grams :: [Gram Text]))) `shouldBe` d

  describe "rws" $ do
    let compare a b = if extract a == extract b then Just (pure (Replace a b)) else Nothing
    prop "produces correct diffs" . forAll (scale (`div` 4) arbitrary) $
      \ (as, bs) -> let tas = toTerm <$> as
                        tbs = toTerm <$> bs
                        diff = free (Free (pure Program :< Indexed (rws compare headF tas tbs :: [Diff Text Category]))) in
        (beforeTerm diff, afterTerm diff) `shouldBe` (Just (cofree (Program :< Indexed tas)), Just (cofree (Program :< Indexed tbs)))
