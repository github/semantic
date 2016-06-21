module Data.Gram where

import Data.DList
import Control.Monad.Random
import Data.Hashable
import Prologue

data Gram label = Gram { stem :: [label], base :: [label] }

serialize :: Gram label -> [label]
serialize gram = stem gram <> base gram


type Bag = DList

instance Hashable label => Hashable (Gram label) where
  hashWithSalt _ = hash
  hash = hash . serialize

instance (Random a, Integral a) => Random (Ratio a) where
  random = first (% 1) . random
