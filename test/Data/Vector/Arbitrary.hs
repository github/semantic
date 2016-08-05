{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Vector.Arbitrary (module Vector) where

import Data.Vector as Vector
import Prologue
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Vector.Vector a) where
  arbitrary = Vector.fromList <$> listOf1 arbitrary
  shrink a = Vector.fromList <$> shrink (Vector.toList a)
