module Control.Abstract.Hole
  ( AbstractHole (..)
  ) where

class AbstractHole a where
  hole :: a
