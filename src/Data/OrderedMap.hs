{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.OrderedMap (
    OrderedMap
  , fromList
  , toList
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

import qualified Data.Maybe as Maybe

-- | An ordered map of keys and values.
newtype OrderedMap key value = OrderedMap { toList :: [(key, value)] }
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Construct an ordered map from a list of pairs of keys and values.
fromList :: [(key, value)] -> OrderedMap key value
fromList = OrderedMap

-- | Return a list of keys from the map.
keys :: OrderedMap key value -> [key]
keys (OrderedMap pairs) = fst <$> pairs

infixl 9 !

-- | Look up a value in the map by key, erroring if it doesn't exist.
(!) :: Eq key => OrderedMap key value -> key -> value
map ! key = Maybe.fromMaybe (error "no value found for key") $ Data.OrderedMap.lookup key map

-- | Look up a value in the map by key, returning Nothing if it doesn't exist.
lookup :: Eq key => key -> OrderedMap key value -> Maybe value
lookup key = Prelude.lookup key . toList

-- | Return the number of pairs in the map.
size :: OrderedMap key value -> Int
size = length . toList

-- | An empty ordered map.
empty :: OrderedMap key value
empty = OrderedMap []

-- | Combine `a` and `b`, picking the values from `a` when keys overlap.
union :: Eq key => OrderedMap key value -> OrderedMap key value -> OrderedMap key value
union a b = OrderedMap $ toList a ++ toList (difference b a)

-- | Union a list of ordered maps.
unions :: Eq key => [OrderedMap key value] -> OrderedMap key value
unions = foldl union empty

-- | Return an ordered map by combining the values from `a` and `b` that have
-- | the same key, dropping any values that are only in one of the maps.
intersectionWith :: Eq key => (a -> b -> c) -> OrderedMap key a -> OrderedMap key b -> OrderedMap key c
intersectionWith combine (OrderedMap a) (OrderedMap b) = OrderedMap $ a >>= (\ (key, value) -> maybe [] (pure . (,) key . combine value) $ Prelude.lookup key b)

-- | Return an ordered map with the pairs from `a` whose key isn't in `b`.
difference :: Eq key => OrderedMap key a -> OrderedMap key b -> OrderedMap key a
difference (OrderedMap a) (OrderedMap b) = OrderedMap $ filter ((`notElem` extant) . fst) a
  where extant = fst <$> b


-- Instances

instance Eq key => Monoid (OrderedMap key value) where
  mempty = fromList []
  mappend = union
