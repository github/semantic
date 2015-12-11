module OrderedMap (
    OrderedMap
  , fromList
  , toList
  , keys
  , (!)
  , OrderedMap.lookup
  , size
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
