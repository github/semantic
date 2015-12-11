module OrderedMap (OrderedMap, fromList, toList) where

data OrderedMap key value = OrderedMap { toList :: [(key, value)] }

fromList :: [(key, value)] -> OrderedMap key value
fromList list = OrderedMap list
