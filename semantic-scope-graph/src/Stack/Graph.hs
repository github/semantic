{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

<<<<<<< Updated upstream
{-# LANGUAGE OverloadedLists #-}
module Stack.Graph (Direction(..), Graph(..), Node, (>>-), (-<<), scope) where
=======
module Stack.Graph (EdgeLabel(..), Graph(..), Node(..), (>>-), (-<<), newScope, singleton, scope) where
>>>>>>> Stashed changes

import           Algebra.Graph.Label (Label)
import           Algebra.Graph.Labelled ((-<), (>-))
import qualified Algebra.Graph.Labelled as Labelled
import           Analysis.Name (Name)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semilattice.Lower
import           Data.String
<<<<<<< Updated upstream
import           Data.Text (Text)
import           GHC.Exts
=======
import           Debug.Trace
import qualified Scope.Types as Scope
>>>>>>> Stashed changes

data Direction = From | To
    deriving (Show, Eq, Ord)

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

newtype Graph a = Graph { unGraph :: Labelled.Graph (Label Direction) a }
  deriving (Show)

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
Graph left >>- Graph right = Graph (Labelled.connect [From] left right)
(-<<) = flip (>>-)

scope :: Name -> Node
scope = Scope . Symbol

singleton :: Node -> Graph Node
singleton node = Graph (Labelled.vertex node)

newScope :: Name -> Map Scope.EdgeLabel [Name] -> Graph Node -> Graph Node
newScope name edges graph = Graph $
  Map.foldrWithKey (\_ scopes graph ->
    foldr (\scope' graph -> Labelled.connect From (Labelled.vertex $ scope name) graph) graph scopes) (unGraph graph) edges

connect :: Name ->

testGraph :: Graph Node
testGraph = (scope "current" >>- declaration "a") >>- (popSymbol "member" >>- declaration "b") >>- (reference "b" >>- pushSymbol "member") >>- (reference "a" >>- root)
