{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Stack.Path
  ( Path (..)
  , startingNode_
  , endingNode_
  , edgeLabels_
  , endingScopeStack_
  , startingSymbolStack_
  , startingScopeStackSize_
  , endingSymbolStack_
  , Edge (..)
  , formatEdge
  , parseEdges
  , StartingSize (..)
  , PathInvariantError (..)
  , checkEdgeInvariants
  , checkNodeInvariants
  , Validity (..)
  , validity
  , Completion (..)
  , completion
  , isIncremental
  ) where


import           Control.Lens.Lens
import           Data.Functor.Tagged
import           Data.Generics.Product
import           Data.Monoid
import           Data.Semigroup (sconcat)
import           Data.Sequence (Seq (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Stack.Graph (Node (..), Symbol)

-- | A partial path through a stack graph. These will be generated
-- from walks through the stack graph, and can be thought of as
-- representing a snapshot of the pathfinding algorithm at a given
-- state.
data Path = Path
  { startingNode           :: Tagged Node
  , endingNode             :: Tagged Node
  , edgeLabels             :: Text -- Encoded
  , startingSymbolStack    :: [Symbol]
  , endingSymbolStack      :: [Symbol]
  , startingScopeStackSize :: StartingSize
  , endingScopeStack       :: [Tag] -- Should this be (Seq (Tagged Node))?
  } deriving (Eq, Show, Generic)

startingNode_ :: Lens' Path (Tagged Node)
startingNode_ = field @"startingNode"

endingNode_ :: Lens' Path (Tagged Node)
endingNode_ = field @"endingNode"

edgeLabels_ :: Lens' Path Text
edgeLabels_ = field @"edgeLabels"

startingSymbolStack_ :: Lens' Path [Symbol]
startingSymbolStack_ = field @"startingSymbolStack"

startingScopeStackSize_ :: Lens' Path StartingSize
startingScopeStackSize_ = field @"startingScopeStackSize"

endingSymbolStack_ :: Lens' Path [Symbol]
endingSymbolStack_ = field @"endingSymbolStack"

endingScopeStack_ :: Lens' Path [Tag]
endingScopeStack_ = field @"endingScopeStack"

-- | This is suitable for conversion from (label, node, node) tuples.
data Edge = Edge
  { sourceNode :: Tagged Node
  , sinkNode   :: Tagged Node
  , label      :: Text
  } deriving (Eq, Show)

parseEdges :: Text -> [Edge]
parseEdges = const []

formatEdge :: Edge -> Text
formatEdge (Edge src sink lab) =
  get src <> ":" <> get sink <> ":" <> lab
    where get = T.pack . show . extract

data StartingSize
  = Zero
  | One
  deriving (Eq, Show)

data PathInvariantError
  = ExpectedEqual (Tagged Node) (Tagged Node)
  | BadStartingNode (Tagged Node)
  | BadEndingNode (Tagged Node)
    deriving (Eq, Show)

-- | If a path's edges list is empty, then its starting node must be
-- the same as its ending node. If a path's edges list is nonempty,
-- then the starting node of the path must be the same as the source
-- node of the first edge in the path, and the ending node of the path
-- must be the same as the sink node of the last edge in the path.
checkEdgeInvariants :: Seq Edge -> Path -> Maybe PathInvariantError
checkEdgeInvariants edges Path{ startingNode, endingNode }
  = let
      check :: Tagged Node -> Tagged Node -> First PathInvariantError
      check a b = if a /= b then pure (ExpectedEqual a b) else mempty
    in getFirst $ case edges of
         Empty
           -> check startingNode endingNode
         Edge { sourceNode, sinkNode } :<| Empty
           -> check startingNode sourceNode <> check endingNode sinkNode
         Edge { sourceNode  } :<| (_ :|> Edge { sinkNode })
           -> check startingNode sourceNode <> check endingNode sinkNode

-- | The starting node of a path must be the root node, an exported
-- scope node, or a reference node. The ending node of a path must be
-- the root node, a jump to scope node, or a definition node.
checkNodeInvariants :: Path -> Maybe PathInvariantError
checkNodeInvariants Path { startingNode, endingNode }
  = getFirst (checkStart <> checkEnd)
    where
      checkStart = case extract startingNode of
        Root            -> mempty
        ExportedScope{} -> mempty
        Reference{}     -> mempty
        _other          -> pure (BadStartingNode startingNode)

      checkEnd = case extract endingNode of
        Root          -> mempty
        JumpToScope{} -> mempty
        Declaration{} -> mempty
        _other        -> pure (BadEndingNode endingNode)

data Validity = Invalid | Valid

instance Semigroup Validity where
  Valid <> Valid = Valid
  _ <> _ = Invalid

-- | A path is valid if all of the following are true:
--
-- 1. If its starting node is a reference, then its starting symbol stack is empty and its starting scope stack size is 0.
-- 2. If its ending node is a definition, then its ending symbol stack and ending scope stack are empty.
-- 3. If its starting scope stack size is 1, then its ending node is a jump to scope node or ignore scope node.
validity :: Path -> Validity
validity p = sconcat [vStart, vEnd, vSize]
  where
    vStart = case extract (startingNode p) of
      Reference{} | null (startingSymbolStack p), startingScopeStackSize p == Zero -> Valid
                  | otherwise -> Invalid
      _otherwise -> Valid

    vEnd = case extract (endingNode p) of
      Declaration{} | null (endingSymbolStack p), null (endingScopeStack p) -> Valid
                    | otherwise -> Invalid
      _otherwise -> Valid

    vSize = case (startingScopeStackSize p, extract (endingNode p)) of
      (One, JumpToScope{}) -> Valid
      (One, IgnoreScope{}) -> Valid
      (One, _)             -> Invalid
      _otherwise           -> Valid

data Completion = Partial | Complete

-- | A path is complete if its starting node is a reference node and its ending node is a definition node. Otherwise it is partial.
completion :: Path -> Completion
completion Path { startingNode = Reference{} :# _, endingNode = Declaration{} :# _} = Complete
completion _                                                                        = Partial

-- | A path is incremental if the source node and sink node of every edge in the path belongs to the same file.
isIncremental :: Path -> Bool
isIncremental = error "TODO: need file support to implement this"
