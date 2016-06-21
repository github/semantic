module Data.Gram where

import Data.DList
import Data.Hashable
import Prologue

data Gram label = Gram { stem :: [label], base :: [label] }

serialize :: Gram label -> [label]
serialize gram = stem gram <> base gram


type Bag = DList

instance Hashable label => Hashable (Gram label) where
  hashWithSalt _ = hash
  hash = hash . serialize
