module Data.Mergeable.Spec where

import Data.Functor.Identity
import Data.Mergeable
import Prologue
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  sequenceAltLaws (arbitrary :: Gen [Char])

sequenceAltLaws :: (Mergeable f, Eq (f a), Show (f a)) => Gen (f a) -> Spec
sequenceAltLaws gen = do
  describe "sequenceAlt" $ do
    prop "identity" . forAll gen $
      \ a -> sequenceAlt (fmap Just a) `shouldBe` Just a
