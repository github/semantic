{-# LANGUAGE DataKinds #-}
module Data.RandomWalkSimilarity.Spec where

import Category
import Data.DList as DList hiding (toList)
import Data.RandomWalkSimilarity
import Data.Record
import qualified Data.Vector.Arbitrary as Vector
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
      \ (term, p, q) -> pqGrams (rhead . headF) p q (toTerm term :: Term Text (Record '[Text])) `shouldSatisfy` all ((== p) . length . stem)

    prop "produces grams with bases of the specified width" . forAll (arbitrary `suchThat` (\ (_, p, q) -> p > 0 && q > 0)) $
      \ (term, p, q) -> pqGrams (rhead . headF) p q (toTerm term :: Term Text (Record '[Text])) `shouldSatisfy` all ((== q) . length . base)

  describe "featureVector" $ do
    prop "produces a vector of the specified dimension" . forAll (arbitrary `suchThat` ((> 0) . Prologue.snd)) $
      \ (grams, d) -> length (featureVector d (fromList (grams :: [Gram Text]))) `shouldBe` d

  describe "rws" $ do
    let compare a b = if extract a == extract b then Just (pure (Replace a b)) else Nothing
    prop "produces correct diffs" . forAll (scale (`div` 4) arbitrary) $
      \ (as, bs) -> let tas = toTerm <$> as
                        tbs = toTerm <$> bs
                        diff = free (Free (pure (Program .: Vector.singleton 0 .: RNil) :< Indexed (rws compare tas tbs :: [Diff Text (Record '[Category, Vector.Vector Double])]))) in
        (beforeTerm diff, afterTerm diff) `shouldBe` (Just (cofree ((Program .: Vector.singleton 0 .: RNil) :< Indexed tas)), Just (cofree ((Program .: Vector.singleton 0 .: RNil) :< Indexed tbs)))
