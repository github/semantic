{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.These.Arbitrary where

import Data.These
import Prologue
import Test.QuickCheck

instance (Arbitrary a, Arbitrary b) => Arbitrary (These a b) where
  arbitrary = oneof [ This <$> arbitrary
                    , That <$> arbitrary
                    , These <$> arbitrary <*> arbitrary ]
  shrink = these (fmap This . shrink) (fmap That . shrink) (\ a b -> (This <$> shrink a) ++ (That <$> shrink b) ++ (These <$> shrink a <*> shrink b))
