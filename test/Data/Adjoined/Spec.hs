module Data.Adjoined.Spec where

import Data.Adjoined
import Test.QuickCheck


instance Arbitrary a => Arbitrary (Adjoined a) where
  arbitrary = fromList <$> arbitrary
