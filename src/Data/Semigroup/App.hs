{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Semigroup.App where

import Control.Applicative

newtype AppMerge f a = AppMerge { runAppMerge :: f a }
  deriving (Alternative, Applicative, Bounded, Enum, Eq, Foldable, Functor, Monad, Ord, Show, Traversable)
