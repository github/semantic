{-# OPTIONS_GHC -fno-warn-orphans #-}
module Patch.Arbitrary where

import Patch
import Prologue
import Test.QuickCheck

patchOf :: Gen a -> Gen a -> Gen (Patch a)
patchOf l r = oneof
  [ Insert <$> r
  , Delete <$> l
  , Replace <$> l <*> r
  ]

instance Arbitrary a => Arbitrary (Patch a) where
  arbitrary = patchOf arbitrary arbitrary

  shrink patch = traverse shrink patch
