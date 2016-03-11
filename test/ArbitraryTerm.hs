module ArbitraryTerm where

import Category
import Syntax
import Term
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.OrderedMap as Map
import qualified Data.List as List
import qualified Data.Set as Set
import GHC.Generics
import Test.QuickCheck hiding (Fixed)
import Data.Text.Arbitrary ()

newtype ArbitraryTerm a annotation = ArbitraryTerm (annotation, Syntax a (ArbitraryTerm a annotation))
  deriving (Show, Eq, Generic)

unTerm :: ArbitraryTerm a annotation -> Term a annotation
unTerm = unfold unpack
  where unpack (ArbitraryTerm (annotation, syntax)) = (annotation, syntax)

instance (Eq a, Eq annotation, Arbitrary a, Arbitrary annotation) => Arbitrary (ArbitraryTerm a annotation) where
  arbitrary = scale (`div` 2) $ sized (\ x -> boundedTerm x x) -- first indicates the cube of the max length of lists, second indicates the cube of the max depth of the tree
    where boundedTerm maxLength maxDepth = ArbitraryTerm <$> ((,) <$> arbitrary <*> boundedSyntax maxLength maxDepth)
          boundedSyntax _ maxDepth | maxDepth <= 0 = liftM Leaf arbitrary
          boundedSyntax maxLength maxDepth = frequency
            [ (12, liftM Leaf arbitrary),
              (1, liftM Indexed $ take maxLength <$> listOf (smallerTerm maxLength maxDepth)),
              (1, liftM Fixed $ take maxLength <$> listOf (smallerTerm maxLength maxDepth)),
              (1, liftM (Keyed . Map.fromList) $ take maxLength <$> listOf (arbitrary >>= (\x -> (,) x <$> smallerTerm maxLength maxDepth))) ]
          smallerTerm maxLength maxDepth = boundedTerm (div maxLength 3) (div maxDepth 3)
  shrink term@(ArbitraryTerm (annotation, syntax)) = (++) (subterms term) $ filter (/= term) $
    ArbitraryTerm <$> ((,) <$> shrink annotation <*> case syntax of
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
