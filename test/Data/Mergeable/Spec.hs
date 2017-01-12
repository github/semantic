{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Data.Mergeable.Spec where

import Data.Functor.Identity
import Data.Functor.Listable
import Data.Mergeable
import Data.String (String)
import GHC.Show
import Prologue
import Syntax
import Test.Hspec
import Test.Hspec.LeanCheck
import Test.LeanCheck

spec :: Spec
spec = parallel $ do
  describe "[]" $ do
    withAlternativeInstances sequenceAltLaws (tiers :: [Tier String])
    withAlternativeInstances mergeLaws (tiers :: [Tier String])
  describe "Maybe" $ do
    withAlternativeInstances sequenceAltLaws (tiers :: [Tier (Maybe Char)])
    withAlternativeInstances mergeLaws (tiers :: [Tier (Maybe Char)])
  describe "Identity" $ do
    withAlternativeInstances sequenceAltLaws (Identity `mapT` tiers :: [Tier (Identity Char)])
    withAlternativeInstances mergeLaws (Identity `mapT` tiers :: [Tier (Identity Char)])
  describe "Syntax" $ do
    withAlternativeInstances sequenceAltLaws (tiers :: [Tier (Syntax Char Char)])
    withAlternativeInstances mergeLaws (tiers :: [Tier (Syntax Char Char)])

  prop "subsumes catMaybes/Just" $
    \ a -> sequenceAlt a `shouldBe` pure (catMaybes (a :: [Maybe Char]))

mergeLaws :: forall f g a. (Mergeable f, Alternative g, Eq (g (f a)), Show (f a), Show (g (f a))) => [Tier (f a)] -> [Tier (Blind (a -> g a))] -> Spec
mergeLaws value function = describe "merge" $ do
  prop "identity" . forAll value $
    \ a -> merge pure a `shouldNotBe` (empty :: g (f a))

  prop "relationship with sequenceAlt" . forAll (value >< function) $
    \ (a, f) -> merge (getBlind f) a `shouldBe` sequenceAlt (fmap (getBlind f) a)

sequenceAltLaws :: forall f g a. (Mergeable f, Alternative g, Eq (g (f a)), Show (f a), Show (g (f a))) => [Tier (f a)] -> [Tier (Blind (a -> g a))] -> Spec
sequenceAltLaws value function = describe "sequenceAlt" $ do
  prop "identity" . forAll value $
    \ a -> sequenceAlt (pure <$> a) `shouldNotBe` (empty :: g (f a))

  prop "relationship with merge" . forAll (productWith ((Blind .) . fmap . getBlind) function value :: [Tier (Blind (f (g a)))]) $
    \ a -> sequenceAlt (getBlind a) `shouldBe` merge identity (getBlind a)


withAlternativeInstances :: forall f a. (Listable a, Eq (f a), Show (f a)) => (forall g. (Alternative g, Eq (g (f a)), Show (g (f a))) => [Tier (f a)] -> [Tier (Blind (a -> g a))] -> Spec) -> [Tier (f a)] -> Spec
withAlternativeInstances laws gen = do
  describe "[]" $ laws gen (fmap const `mapT` tiers :: [Tier (Blind (a -> [a]))])
  describe "Maybe" $ laws gen (fmap const `mapT` tiers :: [Tier (Blind (a -> Maybe a))])


newtype Blind a = Blind { getBlind :: a }
  deriving Functor

instance Listable a => Listable (Blind a) where
  tiers = Blind `mapT` tiers

instance Show (Blind a) where
  showsPrec _ _ = showString "*"
