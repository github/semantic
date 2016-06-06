{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArbitraryTerm where

import Data.Text.Arbitrary ()
import Data.These.Arbitrary ()
import Prologue hiding (fst, snd)
import Source hiding ((++))
import Test.QuickCheck hiding (Fixed)

data CategorySet = A | B | C | D deriving (Eq, Show)

instance Arbitrary CategorySet where
  arbitrary = elements [ A, B, C, D ]

instance Arbitrary a => Arbitrary (Source a) where
  arbitrary = Source.fromList <$> arbitrary
