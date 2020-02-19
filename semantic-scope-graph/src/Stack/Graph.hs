{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Graph (EdgeLabel(..), Graph(..), Node, (>>-), (-<<)) where

import           Algebra.Graph.Labelled ((-<), (>-))
import qualified Algebra.Graph.Labelled as Labelled
import           Data.Semilattice.Lower
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

instance Lower Node where
  lowerBound = Root

newtype Graph a = Graph { unGraph :: Labelled.Graph EdgeLabel a }
  deriving (Show, Eq)

instance Lower a => Lower (Graph a) where
  lowerBound = Graph (Labelled.vertex lowerBound)

(>>-) :: a -> a -> Labelled.Graph EdgeLabel a
left >>- right = left -< From >- right

(-<<) :: a -> a -> Labelled.Graph EdgeLabel a
left -<< right = left -< To >- right

(>>>-) :: Labelled.Graph EdgeLabel a -> Labelled.Graph EdgeLabel a -> Labelled.Graph EdgeLabel a
left >>>- right = Labelled.connect From left right

testGraph :: Graph Node
testGraph = Graph $ (Scope "current" >>- Declaration "a") >>>- (PopSymbol "member" >>- Declaration "b") >>>- (Reference "b" >>- PushSymbol "member") >>>- (Reference "a" >>- Root)
