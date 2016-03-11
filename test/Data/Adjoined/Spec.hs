module Data.Adjoined.Spec where

import Data.Adjoined
import Data.Coalescent
import Test.QuickCheck


instance Arbitrary a => Arbitrary (Adjoined a) where
  arbitrary = fromList <$> arbitrary

newtype Separated a = Separated { unSeparated :: a }
  deriving (Eq, Functor, Show)

instance Arbitrary a => Arbitrary (Separated a) where
  arbitrary = Separated <$> arbitrary

instance Coalescent (Separated a) where
  coalesce _ _ = Nothing
