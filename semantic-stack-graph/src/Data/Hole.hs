module Data.Hole
  ( AbstractHole (..)
  ) where

class AbstractHole a where
  hole :: a
