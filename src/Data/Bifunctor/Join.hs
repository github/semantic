{-# LANGUAGE FlexibleInstances #-}
module Data.Bifunctor.Join where

import Data.Bifunctor
import Data.Bifunctor.These

newtype Join p a = Join { runJoin :: p a a }

instance Bifunctor p => Functor (Join p) where
  fmap f = Join . bimap f f . runJoin

instance Foldable (Join These) where
  foldMap f = these id id mappend . bimap f f . runJoin
