{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE OverloadedLists #-}
module Stack.Graph
  ( Graph(..)
  , Node(..)
  , Symbol(..)
  , (>>-)
  , (-<<)
  , singleton
  -- * Reexports
  , Class.empty
  , Class.vertex
  , Class.overlay
  , Class.connect
  -- * Smart constructors
  , scope
  , newScope
  , declaration
  , reference
  , popSymbol
  , pushSymbol
  , root
  , testGraph
  ) where

import qualified Algebra.Graph as Algebraic
import qualified Algebra.Graph.Class as Class
import           Analysis.Name (Name)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semilattice.Lower
import           Data.String
import qualified Scope.Types as Scope

newtype Symbol = Symbol Name
    deriving (IsString, Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

instance Lower Node where
  lowerBound = Root

newtype Graph a = Graph { unGraph :: Algebraic.Graph a }
  deriving (Eq, Show)

instance Class.Graph (Stack.Graph.Graph a) where
  type Vertex (Stack.Graph.Graph a) = a
  empty = Graph Class.empty
  vertex = Graph . Class.vertex
  overlay (Graph a) (Graph b) = Graph (Class.overlay a b)
  connect (Graph a) (Graph b) = Graph (Class.overlay a b)

instance Lower a => Lower (Graph a) where
  lowerBound = Graph (Algebraic.vertex lowerBound)

scope, declaration, popSymbol, reference, pushSymbol :: Symbol -> Graph Node
scope = Class.vertex . Scope
declaration = Class.vertex . Declaration
reference = Class.vertex . Reference
popSymbol = Class.vertex . PopSymbol
pushSymbol = Class.vertex . PushSymbol

root :: Graph Node
root = Graph (Algebraic.vertex Root)

(>>-), (-<<) :: Graph a -> Graph a -> Graph a
Graph left >>- Graph right = Graph (Algebraic.connect left right)
(-<<) = flip (>>-)

singleton :: Node -> Graph Node
singleton = Class.vertex

newScope :: Name -> Map Scope.EdgeLabel [Name] -> Graph Node -> Graph Node
newScope name edges graph =
  Map.foldrWithKey (\_ scopes graph ->
    foldr (\scope' graph -> (scope (Symbol name)) >>- (scope scope') >>- graph) graph scopes) graph ((fmap Symbol) <$> edges)

testGraph :: Graph Node
testGraph = (scope "current" >>- declaration "a") >>- (popSymbol "member" >>- declaration "b") >>- (reference "b" >>- pushSymbol "member") >>- (reference "a" >>- root)
