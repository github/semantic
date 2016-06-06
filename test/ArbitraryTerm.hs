{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArbitraryTerm where

import Category
import Data.Text.Arbitrary ()
import Data.These.Arbitrary ()
import Info
import Prologue hiding (fst, snd)
import Range
import Source hiding ((++))
import Syntax
import Test.QuickCheck hiding (Fixed)

data CategorySet = A | B | C | D deriving (Eq, Show)

instance Arbitrary CategorySet where
  arbitrary = elements [ A, B, C, D ]

instance Arbitrary a => Arbitrary (Source a) where
  arbitrary = Source.fromList <$> arbitrary

arbitraryLeaf :: Gen (Source Char, Info, Syntax (Source Char) f)
arbitraryLeaf = toTuple <$> arbitrary
  where toTuple string = (string, Info (Range 0 $ length string) StringLiteral 1 0, Leaf string)
