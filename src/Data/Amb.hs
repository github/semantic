module Data.Amb where

import Data.List.NonEmpty
import Data.Semigroup

data Amb l r
  = None l
  | Some (NonEmpty r)
  deriving (Eq, Foldable, Functor, Show, Traversable)

instance Semigroup (Amb l r) where
  l <> None _ = l
  None _ <> r = r
  Some as <> Some bs = Some (as <> bs)
