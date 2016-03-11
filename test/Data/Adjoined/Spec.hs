module Data.Adjoined.Spec where

import Data.Adjoined
import Data.Coalescent
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  prop "equality is reflexive" $
    \ a -> a `shouldBe` (a :: Adjoined (Separated Char))

  monoid (arbitrary :: Gen (Adjoined (Separated Char)))

monoid :: (Arbitrary a, Coalescent a, Eq a, Show a) => Gen (Adjoined a) -> Spec
monoid gen =
  describe "Monoid" $ do
    prop "mempty is the left identity" $ forAll gen $
      \ a -> mempty `mappend` a `shouldBe` a

    prop "mempty is the right identity" $ forAll gen $
      \ a -> a `mappend` mempty `shouldBe` a

    prop "mappend is associative" $ forAll gen $
      \ a b c -> (a `mappend` b) `mappend` c `shouldBe` a `mappend` (b `mappend` c)

instance Arbitrary a => Arbitrary (Adjoined a) where
  arbitrary = fromList <$> arbitrary

-- | A wrapper which never coalesces values.
newtype Separated a = Separated { unSeparated :: a }
  deriving (Eq, Functor, Show)

instance Arbitrary a => Arbitrary (Separated a) where
  arbitrary = Separated <$> arbitrary

instance Coalescent (Separated a) where
  coalesce _ _ = Nothing
