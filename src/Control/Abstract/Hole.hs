module Control.Abstract.Hole where

class AbstractHole a where
  hole :: a


data Hole a = Partial | Total a
  deriving (Foldable, Functor, Eq, Ord, Show, Traversable)
