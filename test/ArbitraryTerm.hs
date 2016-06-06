{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArbitraryTerm where

import Category
import Data.Bifunctor.Join
import Data.Functor.Both
import Data.Text.Arbitrary ()
import Data.These
import Info
import Prologue hiding (fst, snd)
import Range
import Source hiding ((++))
import Syntax
import Test.QuickCheck hiding (Fixed)

data CategorySet = A | B | C | D deriving (Eq, Show)

instance Arbitrary CategorySet where
  arbitrary = elements [ A, B, C, D ]

instance Arbitrary a => Arbitrary (Join (,) a) where
  arbitrary = both <$> arbitrary <*> arbitrary
  shrink b = both <$> shrink (fst b) <*> shrink (snd b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (These a b) where
  arbitrary = oneof [ This <$> arbitrary
                    , That <$> arbitrary
                    , These <$> arbitrary <*> arbitrary ]
  shrink = these (fmap This . shrink) (fmap That . shrink) (\ a b -> (This <$> shrink a) ++ (That <$> shrink b) ++ (These <$> shrink a <*> shrink b))

instance Arbitrary a => Arbitrary (Join These a) where
  arbitrary = Join <$> arbitrary
  shrink (Join a) = Join <$> shrink a

instance Arbitrary a => Arbitrary (Source a) where
  arbitrary = Source.fromList <$> arbitrary

arbitraryLeaf :: Gen (Source Char, Info, Syntax (Source Char) f)
arbitraryLeaf = toTuple <$> arbitrary
  where toTuple string = (string, Info (Range 0 $ length string) StringLiteral 1 0, Leaf string)
