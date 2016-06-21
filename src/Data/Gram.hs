module Data.Gram where

import Prologue

data Gram label = Gram { stem :: [label], base :: [label] }

serialize :: Gram label -> [label]
serialize gram = stem gram <> base gram

newtype Bag a = Bag { unBag :: [a] -> [a] }

instance Monoid (Bag a) where
  mempty = Bag (const [])
  mappend = (Bag .) . ((.) `on` unBag)
