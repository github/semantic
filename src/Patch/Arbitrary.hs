{-# OPTIONS_GHC -fno-warn-orphans #-}
module Patch.Arbitrary where

import Patch
import Prologue
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Patch a) where
  arbitrary = oneof [
    Insert <$> arbitrary,
    Delete <$> arbitrary,
    Replace <$> arbitrary <*> arbitrary ]

  shrink patch = traverse shrink patch
