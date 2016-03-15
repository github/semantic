{-# LANGUAGE FlexibleInstances #-}
module Data.Bifunctor.Join where

import Data.Bifunctor
import Data.Bifunctor.These

newtype Join p a = Join { runJoin :: p a a }

instance Bifunctor p => Functor (Join p) where
  fmap f = Join . bimap f f . runJoin

instance Applicative (Join These) where
  pure a = Join $ These a a
  Join a <*> Join b | Just first <- first, Just second <- second = Join $ These first second
                    | Just first <- first = Join $ This first
                    | Just second <- second = Join $ That second
                    | otherwise = Join a <*> Join (swap b)
    where first = maybeFirst a <*> maybeFirst b
          second = maybeSecond a <*> maybeSecond b

instance Foldable (Join These) where
  foldMap f = these id id mappend . bimap f f . runJoin
