{-# OPTIONS_GHC -fno-warn-orphans #-}
module Patch.Arbitrary where

import Patch
import Prologue
import Test.QuickCheck

patchOf :: Gen a -> Gen (Patch a)
patchOf gen = oneof
  [ Insert <$> gen
  , Delete <$> gen
  , Replace <$> gen <*> gen
  ]

instance Arbitrary a => Arbitrary (Patch a) where
  arbitrary = patchOf arbitrary

  shrink patch = traverse shrink patch
