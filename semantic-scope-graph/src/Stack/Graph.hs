{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE OverloadedLists #-}
module Stack.Graph (Direction(..), Graph(..), Node(..), Symbol(..), (>>-), (-<<), scope, newScope, singleton) where

import           Algebra.Graph.Label (Dioid (..), Label, Semiring (..))
import           Algebra.Graph.Labelled ((-<), (>-))
import qualified Algebra.Graph.Labelled as Labelled
import           Analysis.Name (Name)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semilattice.Lower
import           Data.String
import           Data.Text (Text)
import           Debug.Trace
import           GHC.Exts
import qualified Scope.Types as Scope

data Direction = From | To | Bidi | Zero
    deriving (Show, Eq, Ord)
instance Semigroup Direction where
  From <> From = From
  To <> To = To
  Zero <> a = a
  a <> Zero = a
  _ <> _ = Bidi
instance Monoid Direction where
  mempty = Zero
instance Semiring Direction where
  one = Zero
  (<.>) = (<>)
instance Dioid Direction

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

newtype Graph a = Graph { unGraph :: Labelled.Graph Direction a }
  deriving (Eq, Show)

instance Lower a => Lower (Graph a) where
  lowerBound = Graph (Labelled.vertex lowerBound)

scope, declaration, popSymbol, reference, pushSymbol :: Symbol -> Graph Node
scope = Graph . Labelled.vertex . Scope
declaration = Graph . Labelled.vertex . Declaration
reference = Graph . Labelled.vertex . Reference
popSymbol = Graph . Labelled.vertex . PopSymbol
pushSymbol = Graph . Labelled.vertex . PushSymbol
root :: Graph Node
root = Graph (Labelled.vertex Root)

(>>-), (-<<) :: Graph a -> Graph a -> Graph a
Graph left >>- Graph right = Graph (Labelled.connect From left right)
(-<<) = flip (>>-)

singleton :: Node -> Graph Node
singleton node = Graph (Labelled.vertex node)

newScope :: Name -> Map Scope.EdgeLabel [Name] -> Graph Node -> Graph Node
newScope name edges graph =
  Map.foldrWithKey (\_ scopes graph ->
    foldr (\scope' graph -> (scope (Symbol name)) >>- (scope scope') >>- graph) graph scopes) graph ((fmap Symbol) <$> edges)

testGraph :: Graph Node
testGraph = (scope "current" >>- declaration "a") >>- (popSymbol "member" >>- declaration "b") >>- (reference "b" >>- pushSymbol "member") >>- (reference "a" >>- root)
