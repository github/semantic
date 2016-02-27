module Data.Bifunctor.Join where

newtype Join a = Join { runJoin :: (a, a) }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Join where
  pure a = Join (a, a)
  Join (f, g) <*> Join (a, b) = Join (f a, g b)
