module OrderedMap (OrderedMap) where

data OrderedMap key value = OrderedMap [(key, value)]
