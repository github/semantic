module Control.Abstract.Hole
  ( AbstractHole (..)
  , Hole (..)
  , toMaybe
  ) where

import Prologue

class AbstractHole a where
  hole :: a


data Hole context a = Partial context | Total a
  deriving (Foldable, Functor, Eq, Ord, Show, Traversable)

instance Lower context => AbstractHole (Hole context a) where
  hole = Partial lowerBound

toMaybe :: Hole context a -> Maybe a
toMaybe (Partial _) = Nothing
toMaybe (Total a)   = Just a
