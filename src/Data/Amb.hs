module Data.Amb where

import Data.List.NonEmpty

data Amb l r
  = None l
  | Some (NonEmpty r)
  deriving (Eq, Foldable, Functor, Show, Traversable)
