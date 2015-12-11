module OrderedMap (
    OrderedMap
  , fromList
  , toList
  , keys
  , (!)
  ) where

data OrderedMap key value = OrderedMap { toList :: [(key, value)] }
  deriving (Show, Eq, Functor, Foldable, Traversable)

fromList :: [(key, value)] -> OrderedMap key value
fromList list = OrderedMap list

keys :: OrderedMap key value -> [key]
keys (OrderedMap pairs) = fst <$> pairs

infixl 9 !

(!) :: Eq key => OrderedMap key value -> key -> value
OrderedMap pairs ! key = case lookup key pairs of
  Just value -> value
  Nothing -> error "no value found for key"
