module Data.Reprinting.Operator
  ( Operator (..)
  , Direction (..)
  ) where

data Direction
  = Less
  | Greater
    deriving (Show, Eq)

-- | A sum type representing every concievable infix operator a
-- language can define. These are handled by instances of 'Concrete'
-- and given appropriate precedence.
data Operator
  = Add
  | Multiply
  | Subtract
  | Divide
  | Modulus
  | Raise
  | FloorDivide
  | RegexMatch
  | RegexNotMatch
  | LogicalOr
  | LogicalAnd
  | LogicalNot
  | LogicalXor
  | BinaryOr
  | BinaryAnd
  | BinaryXor
  | BinaryComplement
  | NumericNegate
  | LeftShift
  | RightShift
  | Eql
  | StrictEql
  | Compare Direction
  | CompareEql Direction
  | Spaceship
    deriving (Show, Eq)
