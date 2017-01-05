{-# OPTIONS_GHC -fno-warn-orphans #-}
module Patch.Arbitrary where

import Data.Functor.Listable
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

instance Listable1 Patch where
  liftTiers t = liftCons1 t Insert \/ liftCons1 t Delete \/ liftCons2 t t Replace
