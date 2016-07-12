{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Data.OrderedMap (
    OrderedMap(..)
  , fromList
  , keys
  , (!)
  , Data.OrderedMap.lookup
  , size
  , empty
  , union
  , unions
  , intersectionWith
  , difference
  ) where

import Data.Align
import Data.List ((\\))
import qualified Data.List as List
import Data.These
import GHC.Generics
import Prologue hiding (toList, empty)
import Test.QuickCheck

-- | An ordered map of keys and values.
newtype OrderedMap key value = OrderedMap { toList :: [(key, value)] }
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)

-- | Construct an ordered map from a list of pairs of keys and values.
fromList :: [(key, value)] -> OrderedMap key value
fromList = OrderedMap

-- | Return a list of keys from the map.
keys :: OrderedMap key value -> [key]
keys (OrderedMap pairs) = fst <$> pairs

infixl 9 !

-- | Look up a value in the map by key, erroring if it doesn't exist.
(!) :: Eq key => OrderedMap key value -> key -> value
m ! key = fromMaybe (error "no value found for key") $ Data.OrderedMap.lookup key m

-- | Look up a value in the map by key, returning Nothing if it doesn't exist.
lookup :: Eq key => key -> OrderedMap key value -> Maybe value
lookup key = Prologue.lookup key . toList

-- | Return the number of pairs in the map.
size :: OrderedMap key value -> Int
size = length . toList

-- | An empty ordered map.
empty :: OrderedMap key value
empty = OrderedMap []

-- | Combine `a` and `b`, picking the values from `a` when keys overlap.
union :: Eq key => OrderedMap key value -> OrderedMap key value -> OrderedMap key value
union a b = OrderedMap $ toList a <> toList (difference b a)

-- | Union a list of ordered maps.
unions :: Eq key => [OrderedMap key value] -> OrderedMap key value
unions = foldl union empty

-- | Return an ordered map by combining the values from `a` and `b` that have
-- | the same key, dropping any values that are only in one of the maps.
intersectionWith :: Eq key => (a -> b -> c) -> OrderedMap key a -> OrderedMap key b -> OrderedMap key c
intersectionWith combine (OrderedMap a) (OrderedMap b) = OrderedMap $ a >>= (\ (key, value) -> maybe [] (pure . (,) key . combine value) $ Prologue.lookup key b)

-- | Return an ordered map with the pairs from `a` whose key isn't in `b`.
difference :: Eq key => OrderedMap key a -> OrderedMap key b -> OrderedMap key a
difference (OrderedMap a) (OrderedMap b) = OrderedMap $ filter ((`notElem` extant) . fst) a
  where extant = fst <$> b


-- Instances

instance Eq key => Monoid (OrderedMap key value) where
  mempty = fromList []
  mappend = union

instance (Arbitrary key, Arbitrary value) => Arbitrary (OrderedMap key value) where
  arbitrary = fromList <$> arbitrary
  shrink = genericShrink

instance Eq key => Align (OrderedMap key) where
  nil = fromList []
  align a b = OrderedMap $ toKeyValue <$> List.union aKeys bKeys
    where toKeyValue key | key `List.elem` deleted = (key, This $ a ! key)
                         | key `List.elem` inserted = (key, That $ b ! key)
                         | otherwise = (key, These (a ! key) (b ! key))
          aKeys = keys a
          bKeys = keys b
          deleted = aKeys \\ bKeys
          inserted = bKeys \\ aKeys
