{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Bifunctor.Join.Arbitrary where

import Data.Bifunctor.Join
import Data.Functor.Both as Both
import Data.These
import Data.These.Arbitrary ()
import Prologue
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Join These a) where
  arbitrary = Join <$> arbitrary
  shrink (Join a) = Join <$> shrink a

instance Arbitrary a => Arbitrary (Join (,) a) where
  arbitrary = both <$> arbitrary <*> arbitrary
  shrink b = both <$> shrink (Both.fst b) <*> shrink (Both.snd b)
