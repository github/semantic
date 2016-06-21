module Data.Gram where

import Data.Hashable
import Prologue

data Gram label = Gram { stem :: [label], base :: [label] }

serialize :: Gram label -> [label]
serialize gram = stem gram <> base gram

newtype Bag a = Bag { unBag :: [a] -> [a] }

instance Hashable label => Hashable (Gram label) where
  hashWithSalt _ = hash
  hash = hash . serialize

instance Hashable a => Hashable (Bag a) where
  hashWithSalt _ = hash
  hash = hash . ($ mempty) . unBag

instance Monoid (Bag a) where
  mempty = Bag (const [])
  mappend = (Bag .) . ((.) `on` unBag)
