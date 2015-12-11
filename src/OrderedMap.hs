module OrderedMap (OrderedMap, fromList) where

data OrderedMap key value = OrderedMap [(key, value)]

fromList :: [(key, value)] -> OrderedMap key value
fromList list = OrderedMap list
