{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Gram.Arbitrary where

import Data.Gram
import Prologue
import Test.QuickCheck

instance Arbitrary label => Arbitrary (Gram label) where
  arbitrary = Gram <$> arbitrary <*> arbitrary
