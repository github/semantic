{-# LANGUAGE ScopedTypeVariables #-}
module Data.Mergeable.Spec where

import Data.Functor.Identity
import Data.Mergeable
import Prologue
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "[]" $ sequenceAltLaws (arbitrary :: Gen [Char])
  describe "Maybe" $ sequenceAltLaws (arbitrary :: Gen (Maybe Char))
  describe "Identity" $ sequenceAltLaws (Identity <$> arbitrary :: Gen (Identity Char))

sequenceAltLaws :: forall f a. (Arbitrary a, CoArbitrary a, Mergeable f, Eq (f a), Show (f a)) => Gen (f a) -> Spec
sequenceAltLaws gen = do
  describe "Maybe" $ sequenceAltLaws' gen (arbitrary :: Gen (Blind (a -> Maybe a)))

sequenceAltLaws' :: forall f g a. (Arbitrary a, CoArbitrary a, Mergeable f, Alternative g, Eq (f a), Eq (g (f a)), Show (f a), Show (g (f a))) => Gen (f a) -> Gen (Blind (a -> g a)) -> Spec
sequenceAltLaws' value function = do
  describe "sequenceAlt" $ do
    prop "identity" . forAll value $
      \ a -> sequenceAlt (fmap Just a) `shouldBe` Just a

    prop "relationship with merge" . forAll ((,) <$> value <*> function) $
      \ (a, f) -> sequenceAlt (getBlind f <$> a) `shouldBe` merge (getBlind f) a
