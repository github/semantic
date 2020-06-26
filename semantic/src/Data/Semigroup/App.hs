{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Semigroup.App
( App(..)
, AppMerge(..)
) where

import Control.Applicative

-- | 'Semigroup' under '*>'.
newtype App f a = App { runApp :: f a }
  deriving (Alternative, Applicative, Bounded, Enum, Eq, Foldable, Functor, Monad, Ord, Show, Traversable)

instance Applicative f => Semigroup (App f a) where
  App a <> App b = App (a *> b)

-- | 'Semigroup' and 'Monoid' under '<*>' and '<>'.
newtype AppMerge f a = AppMerge { runAppMerge :: f a }
  deriving (Alternative, Applicative, Bounded, Enum, Eq, Foldable, Functor, Monad, Ord, Show, Traversable)

instance (Applicative f, Semigroup a) => Semigroup (AppMerge f a) where
  AppMerge a <> AppMerge b = AppMerge ((<>) <$> a <*> b)

instance (Applicative f, Monoid a, Semigroup a) => Monoid (AppMerge f a) where
  mempty = AppMerge (pure mempty)
  mappend = (<>)
