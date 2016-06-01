{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArbitraryTerm where

import Category
import Data.Bifunctor.Join
import Data.Functor.Both
import Data.Functor.Foldable
import qualified Data.OrderedMap as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Text.Arbitrary ()
import Data.These
import Info
import Patch
import Prologue hiding (fst, snd)
import Range
import Source hiding ((++))
import Syntax
import Term
import Test.QuickCheck hiding (Fixed)

newtype ArbitraryTerm a annotation = ArbitraryTerm (CofreeF (Syntax a) annotation (ArbitraryTerm a annotation))
  deriving (Show, Eq, Generic)

unTerm :: ArbitraryTerm a annotation -> Term a annotation
unTerm = unfold unpack
  where unpack (ArbitraryTerm (annotation :< syntax)) = annotation :< syntax

instance (Eq a, Eq annotation, Arbitrary a, Arbitrary annotation) => Arbitrary (ArbitraryTerm a annotation) where
  arbitrary = scale (`div` 2) $ sized (\ x -> boundedTerm x x) -- first indicates the cube of the max length of lists, second indicates the cube of the max depth of the tree
    where boundedTerm maxLength maxDepth = ArbitraryTerm <$> ((:<) <$> arbitrary <*> boundedSyntax maxLength maxDepth)
          boundedSyntax _ maxDepth | maxDepth <= 0 = Leaf <$> arbitrary
          boundedSyntax maxLength maxDepth = frequency
            [ (12, Leaf <$> arbitrary),
              (1, Indexed . take maxLength <$> listOf (smallerTerm maxLength maxDepth)),
              (1, Fixed . take maxLength <$> listOf (smallerTerm maxLength maxDepth)),
              (1, Keyed . Map.fromList . take maxLength <$> listOf (arbitrary >>= (\x -> (,) x <$> smallerTerm maxLength maxDepth))) ]
          smallerTerm maxLength maxDepth = boundedTerm (div maxLength 3) (div maxDepth 3)
  shrink term@(ArbitraryTerm (annotation :< syntax)) = (subterms term ++) $ filter (/= term) $
    ArbitraryTerm <$> ((:<) <$> shrink annotation <*> case syntax of
      Leaf a -> Leaf <$> shrink a
      Indexed i -> Indexed <$> (List.subsequences i >>= recursivelyShrink)
      Fixed f -> Fixed <$> (List.subsequences f >>= recursivelyShrink)
      Keyed k -> Keyed . Map.fromList <$> (List.subsequences (Map.toList k) >>= recursivelyShrink))

data CategorySet = A | B | C | D deriving (Eq, Show)

instance Categorizable CategorySet where
  categories A = Set.fromList [ Other "a" ]
  categories B = Set.fromList [ Other "b" ]
  categories C = Set.fromList [ Other "c" ]
  categories D = Set.fromList [ Other "d" ]

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

instance Arbitrary a => Arbitrary (Patch a) where
  arbitrary = oneof [
    Insert <$> arbitrary,
    Delete <$> arbitrary,
    Replace <$> arbitrary <*> arbitrary ]

instance Arbitrary a => Arbitrary (Source a) where
  arbitrary = Source.fromList <$> arbitrary

arbitraryLeaf :: Gen (Source Char, Info, Syntax (Source Char) f)
arbitraryLeaf = toTuple <$> arbitrary
  where toTuple string = (string, Info (Range 0 $ length string) mempty 1, Leaf string)
