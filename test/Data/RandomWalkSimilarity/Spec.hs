{-# LANGUAGE DataKinds #-}
module Data.RandomWalkSimilarity.Spec where

import Category
import Data.DList as DList hiding (toList)
import Data.RandomWalkSimilarity
import Data.RandomWalkSimilarity.Arbitrary ()
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
      \ (term, p, q) -> pqGrams p q identity (toTerm term :: Term Text Text) `shouldSatisfy` all ((== p) . length . stem)

    prop "produces grams with bases of the specified length" . forAll (arbitrary `suchThat` (\ (_, p, q) -> p > 0 && q > 0)) $
      \ (term, p, q) -> pqGrams p q identity (toTerm term :: Term Text Text) `shouldSatisfy` all ((== q) . length . base)

  describe "featureVector" $ do
    prop "produces a vector of the specified dimension" . forAll (arbitrary `suchThat` ((> 0) . Prologue.snd)) $
      \ (grams, d) -> length (featureVector d (fromList (grams :: [Gram Text]))) `shouldBe` d

  describe "rws" $ do
    let compare a b = if extract a == extract b then Just (pure (Replace a b)) else Nothing
    let enumerate = zipWith (\ i t -> fmap ((,) i) t) [0..]
    let sort term = let annotation :< (Indexed children) = runCofree term in cofree (annotation :< Indexed (sortOn (fst . extract) children))
    prop "produces correct diffs" $
      \ as bs -> let tas = enumerate $ toTerm <$> as
                     tbs = enumerate $ toTerm <$> bs
                     diff = free (Free (pure (0, Program) :< Indexed (rws compare snd tas tbs :: [Diff Text (Integer, Category)]))) in
        (sort <$> beforeTerm diff, sort <$> afterTerm diff) `shouldBe` (Just (cofree ((0, Program) :< Indexed tas)), Just (cofree ((0, Program) :< Indexed tbs)))
