module Data.Reprinting.Operator
  ( Operator (..)
  ) where

-- | A sum type representing every concievable infix operator a
-- language can define. These are handled by instances of 'Concrete'
-- and given appropriate precedence.
data Operator
  = Add
  | Multiply
  | Subtract
    deriving (Show, Eq)
