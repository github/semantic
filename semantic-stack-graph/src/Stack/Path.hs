{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
module Stack.Path
  ( Path (..)
  , Edge (..)
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


import Control.Lens.Getter
import Data.Functor.Tagged
import Data.Monoid
import Data.Semigroup (sconcat)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Stack.Node

-- | A partial path through a stack graph. These will be generated
-- from walks through the stack graph, and can be thought of as
-- representing a snapshot of the pathfinding algorithm at a given
-- state.
data Path = Path
  { startingNode           :: Node
  , endingNode             :: Node
  , edges                  :: Seq Edge
  , startingSymbolStack    :: [Symbol]
  , endingSymbolStack      :: [Symbol]
  , startingScopeStackSize :: StartingSize
  , endingScopeStack       :: [Tag]
  } deriving (Eq, Show)

data Edge = Edge
  { sourceNode :: Node
  , sinkNode   :: Node
  , label      :: Text
  } deriving (Eq, Show)

data StartingSize
  = Zero
  | One
  deriving (Eq, Show, Ord, Enum)

data PathInvariantError
  = ExpectedEqual (Node) (Node)
  | BadStartingNode (Node)
  | BadEndingNode (Node)
    deriving (Eq, Show)

-- | If a path's edges list is empty, then its starting node must be
-- the same as its ending node. If a path's edges list is nonempty,
-- then the starting node of the path must be the same as the source
-- node of the first edge in the path, and the ending node of the path
-- must be the same as the sink node of the last edge in the path.
checkEdgeInvariants :: Path -> Maybe PathInvariantError
checkEdgeInvariants Path{ startingNode, endingNode, edges }
  = let
      check :: Node -> Node -> First PathInvariantError
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
      checkStart = case startingNode^.info_.type_ of
        Root            -> mempty
        ExportedScope{} -> mempty
        Reference{}     -> mempty
        _other          -> pure (BadStartingNode startingNode)

      checkEnd = case endingNode^.info_.type_ of
        Root          -> mempty
        JumpToScope{} -> mempty
        Definition{}  -> mempty
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
    vStart = case p ^. to startingNode.info_.type_ of
      Reference{} | null (startingSymbolStack p), startingScopeStackSize p == Zero -> Valid
                  | otherwise -> Invalid
      _otherwise -> Valid

    vEnd = case p ^. to endingNode.info_.type_ of
      Definition{} | null (endingSymbolStack p), null (endingScopeStack p) -> Valid
                    | otherwise -> Invalid
      _otherwise -> Valid

    vSize = case (startingScopeStackSize p, p ^. to endingNode.info_.type_) of
      (One, JumpToScope{}) -> Valid
      (One, IgnoreScope{}) -> Valid
      (One, _)             -> Invalid
      _otherwise           -> Valid

data Completion = Partial | Complete

-- | A path is complete if its starting node is a reference node and its ending node is a definition node. Otherwise it is partial.
completion :: Path -> Completion
completion p = case (p^.to startingNode.info_.type_, p^.to endingNode.info_.type_) of
  (Reference{}, Definition{}) -> Complete
  _                           -> Partial

-- | A path is incremental if the source node and sink node of every edge in the path belongs to the same file.
isIncremental :: Path -> Bool
isIncremental = error "TODO: need file support to implement this"
