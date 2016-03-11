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

instance Arbitrary a => Arbitrary (Adjoined a) where
  arbitrary = fromList <$> arbitrary

-- | A wrapper which never coalesces values.
newtype Separated a = Separated { unSeparated :: a }
  deriving (Eq, Functor, Show)

instance Arbitrary a => Arbitrary (Separated a) where
  arbitrary = Separated <$> arbitrary

instance Coalescent (Separated a) where
  coalesce _ _ = Nothing
