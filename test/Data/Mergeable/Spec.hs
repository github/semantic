{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Data.Mergeable.Spec where

import Data.Functor.Identity
import Data.Mergeable
import Prologue
import Syntax
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "[]" $ do
    withAlternativeInstances sequenceAltLaws (arbitrary :: Gen [Char])
    withAlternativeInstances mergeLaws (arbitrary :: Gen [Char])
  describe "Maybe" $ do
    withAlternativeInstances sequenceAltLaws (arbitrary :: Gen (Maybe Char))
    withAlternativeInstances mergeLaws (arbitrary :: Gen (Maybe Char))
  describe "Identity" $ do
    withAlternativeInstances sequenceAltLaws (Identity <$> arbitrary :: Gen (Identity Char))
    withAlternativeInstances mergeLaws (Identity <$> arbitrary :: Gen (Identity Char))
  describe "Syntax" $ do
    withAlternativeInstances sequenceAltLaws (sized (syntaxOfSize (const arbitrary)) :: Gen (Syntax Char Char))
    withAlternativeInstances mergeLaws (sized (syntaxOfSize (const arbitrary)) :: Gen (Syntax Char Char))

mergeLaws :: forall f g a. (Mergeable f, Alternative g, Eq (g (f a)), Show (f a), Show (g (f a))) => Gen (f a) -> Gen (Blind (a -> g a)) -> Spec
mergeLaws value function = describe "merge" $ do
  prop "relationship with sequenceAlt" . forAll ((,) <$> value <*> function) $
    \ (a, f) -> merge (getBlind f) a `shouldBe` sequenceAlt (fmap (getBlind f) a)

sequenceAltLaws :: forall f g a. (Mergeable f, Alternative g, Eq (g (f a)), Show (f a), Show (g (f a))) => Gen (f a) -> Gen (Blind (a -> g a)) -> Spec
sequenceAltLaws value function = do
  describe "sequenceAlt" $ do
    prop "identity" . forAll value $
      \ a -> sequenceAlt (pure <$> a) `shouldBe` (pure a :: g (f a))

    prop "relationship with merge" . forAll ((,) <$> value <*> function) $
      \ (a, f) -> sequenceAlt (getBlind f <$> a) `shouldBe` merge (getBlind f) a


withAlternativeInstances :: forall f a. (Arbitrary a, CoArbitrary a, Eq (f a), Show (f a)) => (forall g. (Alternative g, Eq (g (f a)), Show (g (f a))) => Gen (f a) -> Gen (Blind (a -> g a)) -> Spec) -> Gen (f a) -> Spec
withAlternativeInstances laws gen = do
  describe "Maybe" $ laws gen (arbitrary :: Gen (Blind (a -> Maybe a)))
