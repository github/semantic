{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Stack.Path
  ( Path (..)
  , Compatibility (..)
  , compatibility
  , concatenate
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


import Data.Functor.Tagged
import Data.Monoid
import Data.Semigroup (sconcat)
import Data.Sequence (Seq (..), (|>))
import Data.Text (Text)
import Stack.Graph (Node (..), Symbol)
import Data.Generics.Product
import Control.Lens.Getter ((^.))
import GHC.Generics
import Data.List (isPrefixOf)

-- | A partial path through a stack graph. These will be generated
-- from walks through the stack graph, and can be thought of as
-- representing a snapshot of the pathfinding algorithm at a given
-- state.
data Path = Path
  { startingNode           :: Tagged Node
  , endingNode             :: Tagged Node
  , edges                  :: Seq Edge
  , startingSymbolStack    :: [Symbol]
  , endingSymbolStack      :: [Symbol]
  , startingScopeStackSize :: StartingSize
  , endingScopeStack       :: [Tag]
  } deriving (Eq, Show, Generic)

data Edge = Edge
  { sourceNode :: Tagged Node
  , sinkNode   :: Tagged Node
  , label      :: Text
  } deriving (Eq, Show)

data StartingSize
  = Zero
  | One
  deriving (Eq, Show, Ord, Enum)

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
checkEdgeInvariants :: Path -> Maybe PathInvariantError
checkEdgeInvariants Path{ startingNode, endingNode, edges }
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

data Completion
  = Partial
  | Complete
    deriving (Eq, Show)

-- | A path is complete if its starting node is a reference node and its ending node is a definition node. Otherwise it is partial.
completion :: Path -> Completion
completion Path { startingNode = Reference{} :# _, endingNode = Declaration{} :# _} = Complete
completion _                                                                        = Partial

-- | A path is incremental if the source node and sink node of every edge in the path belongs to the same file.
isIncremental :: Path -> Bool
isIncremental = error "TODO: need file support to implement this"

data Compatibility
  = Compatible
  | Incompatible
  deriving (Eq, Show)

compatibility :: Path -> Path -> Compatibility
compatibility left right
  -- Two paths 'left' and 'right' are compatible with each other if all the following are true:
  | and @[] [nodesCompatible, stackPrefix, hasElements] = Compatible
  | otherwise = Incompatible
  where
    -- Any of the following are true:
    nodesCompatible =
      -- The ending node of 'left' and the starting node of 'right' are both the root node.
      let bothRootNode = left ^. field @"endingNode".contents == Root && right ^. field @"startingNode".contents == Root
      -- The ending node of 'left' and the starting node of 'right' are both scope references, and both refer to the same scope
          bothSameScope = case (endingNode left, startingNode right) of
            -- TODO: determining "same scope" by symbol comparison is rough
            -- should we be comparing tags as well?
            (Scope s1 :# _, Scope s2 :# _) -> s1 == s2
            _ -> False
       in bothRootNode || bothSameScope
    -- The starting symbol stack of 'right' is a prefix of the ending symbol stack of 'left'.
    stackPrefix = startingSymbolStack right `isPrefixOf` endingSymbolStack left
    -- The ending scope stack of 'left' has at least as many elements as the starting scope stack size of 'right'.
    hasElements = length (endingScopeStack left) >= fromEnum (startingScopeStackSize right)

concatenate :: Path -> Path -> Maybe Path
concatenate left right
  -- Incompatible paths cannot be concatenated.
  | compatibility left right == Incompatible = Nothing
  -- If left and right are compatible with each other, you can concatenate them together, yielding a new path:
  | otherwise =
    -- The new path's starting node, starting symbol stack, and starting scope stack size are the same as left.
    let (newStartingNode, newStartingSymbolStack, newStartingScopeStackSize) = (startingNode left, startingSymbolStack left, startingScopeStackSize left)
        -- The new path's edge list is the concatenation of left's and right's edge lists.
        allEdges = edges left <> edges right
        -- The new path's ending symbol stack is the value of new symbol stack after doing the following:
        newEndingSymbolStack =
          -- Let new symbol stack be a copy of left's ending symbol stack.
          let newSymbolStack = endingSymbolStack left
              -- Remove right's starting symbol stack from the beginning of new symbol stack. (This must succeed because the two input paths are compatible.)
              withoutRight = drop (length (startingSymbolStack right)) newSymbolStack
           in -- Prepend a copy of right's ending symbol stack to the beginning of new symbol stack.
              endingSymbolStack right <> withoutRight
        -- The new path's ending scope stack is the value of new scope stack after doing the following:
        newEndingScopeStack =
          -- Let new scope stack be a copy of left's ending scope stack.
          let newScopeStack = endingScopeStack left
              -- If right's starting scope stack size is 1, pop resolved scope identifier from the beginning of new scope stack.
              popped = if startingScopeStackSize right == One then drop 1 newScopeStack else newScopeStack
           in -- Prepend a copy of right's ending scope stack to the beginning of new scope stack.
              endingScopeStack right <> popped
        -- The new path's ending node is the same as right's.
        newEndingNode = endingNode right
        newEdges = case newEndingNode of
          -- If right's ending node is a jump to scope node node, then:
          JumpToScope :# _ ->
            -- Let jump edge be a new edge whose:
            let jumpEdge =
                  Edge
                    { sourceNode = newEndingNode, -- source node is node

                        -- PT TODO: we don't appear to have scope identifiers attached to exported scope nodes
                        -- Also: calling unsafeTagged here is a sin, but this function should be pure, so whatever.
                        -- If anyone haa a better idea, hit me up.

                      -- sink node is an exported scope node whose scope identifier is resolved scope identifier
                      sinkNode = unsafeTagged ExportedScope,
                      -- label is `jump``
                      label = "jump"
                    }
             in -- Append jump edge to the new path.
                (allEdges |> jumpEdge)
          -- Otherwise, do nothing
          _ -> allEdges
     in Just
          Path
            { startingNode = newStartingNode,
              endingNode = newEndingNode,
              edges = newEdges,
              startingSymbolStack = newStartingSymbolStack,
              endingSymbolStack = newEndingSymbolStack,
              startingScopeStackSize = newStartingScopeStackSize,
              endingScopeStack = newEndingScopeStack
            }
