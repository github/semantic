module Control.Abstract.Hole
  ( AbstractHole (..)
  , Hole (..)
  , toMaybe
  ) where

class AbstractHole a where
  hole :: a


data Hole a = Partial | Total a
  deriving (Foldable, Functor, Eq, Ord, Show, Traversable)

instance AbstractHole (Hole a) where
  hole = Partial

toMaybe :: Hole a -> Maybe a
toMaybe Partial = Nothing
toMaybe (Total a) = Just a
