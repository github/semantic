module Data.Bifunctor.Join where

newtype Join a = Join { runJoin :: (a, a) }
  deriving (Eq, Show, Functor, Foldable, Traversable)
