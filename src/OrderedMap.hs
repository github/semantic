module OrderedMap (
    OrderedMap
  , fromList
  , toList
  , keys
  , (!)
  , OrderedMap.lookup
  , size
  , empty
  , union
  , unions
  , intersectionWith
  ) where

data OrderedMap key value = OrderedMap { toList :: [(key, value)] }
  deriving (Show, Eq, Functor, Foldable, Traversable)

fromList :: [(key, value)] -> OrderedMap key value
fromList list = OrderedMap list

keys :: OrderedMap key value -> [key]
keys (OrderedMap pairs) = fst <$> pairs

infixl 9 !

(!) :: Eq key => OrderedMap key value -> key -> value
map ! key = case OrderedMap.lookup key map of
  Just value -> value
  Nothing -> error "no value found for key"

lookup :: Eq key => key -> OrderedMap key value -> Maybe value
lookup key = Prelude.lookup key . toList

size :: OrderedMap key value -> Int
size = length . toList

empty :: OrderedMap key value
empty = OrderedMap []

union :: Eq key => OrderedMap key value -> OrderedMap key value -> OrderedMap key value
union (OrderedMap a) (OrderedMap b) = OrderedMap $ a ++ filter (not . (`elem` extant) . fst) b
  where extant = fst <$> a

unions :: Eq key => [OrderedMap key value] -> OrderedMap key value
unions = foldl union empty

intersectionWith :: Eq key => (a -> b -> c) -> OrderedMap key a -> OrderedMap key b -> OrderedMap key c
intersectionWith combine (OrderedMap a) (OrderedMap b) = OrderedMap $ a >>= (\ (key, value) -> maybe [] (pure . ((,) key) . combine value) $ Prelude.lookup key b)
