{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Semigroup.App where

import Control.Applicative
import Data.Semigroup

-- | 'Semigroup' under '<*>' and '<>'.
newtype AppMerge f a = AppMerge { runAppMerge :: f a }
  deriving (Alternative, Applicative, Bounded, Enum, Eq, Foldable, Functor, Monad, Ord, Show, Traversable)

instance (Applicative f, Semigroup a) => Semigroup (AppMerge f a) where
  AppMerge a <> AppMerge b = AppMerge ((<>) <$> a <*> b)
