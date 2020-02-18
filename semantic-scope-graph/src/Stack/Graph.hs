{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Graph (EdgeLabel(..), Graph(..), empty, Node, (>>-), (-<<), (>>>-)) where

import           Algebra.Graph.Labelled ((-<), (>-))
import qualified Algebra.Graph.Labelled as Labelled
import           Data.String
import           Data.Text (Text)

data EdgeLabel = Empty | From | To
    deriving (Show, Eq, Ord)
instance Monoid EdgeLabel where
    mempty = Empty

instance Semigroup EdgeLabel where
    _ <> b = b

newtype Symbol = Symbol Text
    deriving (IsString, Show, Eq)

data Node = Root
  | Declaration Symbol
  | Reference Symbol
  | PushSymbol Symbol
  | PopSymbol Symbol
  | PushScope
  | Scope Symbol
  | ExportedScope
  | JumpToScope
  | IgnoreScope
  deriving (Show, Eq)

newtype Graph a = Graph { unGraph :: Labelled.Graph EdgeLabel a }
  deriving (Show, Eq)

empty :: Graph Node
empty = Graph (Labelled.vertex Root)

(>>-) :: a -> a -> Labelled.Graph EdgeLabel a
left >>- right = left -< From >- right

(-<<) :: a -> a -> Labelled.Graph EdgeLabel a
left -<< right = left -< To >- right

(>>>-) :: Labelled.Graph EdgeLabel a -> Labelled.Graph EdgeLabel a -> Labelled.Graph EdgeLabel a
left >>>- right = Labelled.connect From left right
