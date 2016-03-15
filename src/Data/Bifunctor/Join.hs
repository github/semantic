module Data.Bifunctor.Join where

import Data.Bifunctor

newtype Join p a = Join { runJoin :: p a a }

instance Bifunctor p => Functor (Join p) where
  fmap f = Join . bimap f f . runJoin
