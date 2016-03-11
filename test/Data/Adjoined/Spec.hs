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

  describe "Monoid" $ do
    let memptySep = mempty :: Adjoined (Separated Char)
    prop "mempty is the left identity" $
      \ a -> memptySep `mappend` a `shouldBe` a

    prop "mempty is the right identity" $
      \ a -> a `mappend` memptySep `shouldBe` a

instance Arbitrary a => Arbitrary (Adjoined a) where
  arbitrary = fromList <$> arbitrary

-- | A wrapper which never coalesces values.
newtype Separated a = Separated { unSeparated :: a }
  deriving (Eq, Functor, Show)

instance Arbitrary a => Arbitrary (Separated a) where
  arbitrary = Separated <$> arbitrary

instance Coalescent (Separated a) where
  coalesce _ _ = Nothing
