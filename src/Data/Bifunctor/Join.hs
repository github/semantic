module Data.Bifunctor.Join where

newtype Join p a = { runJoin :: p a a }

instance Bifunctor p => Functor (Join p) where
  fmap f = Join . bimap f f . runJoin
