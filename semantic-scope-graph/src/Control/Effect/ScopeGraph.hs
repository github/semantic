{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The ScopeGraph effect is used to build up a scope graph over
-- the lifetime of a monadic computation. The name is meant to evoke
-- physically sketching the hierarchical outline of a graph.
module Control.Effect.ScopeGraph
  ( ScopeGraph
  , ScopeGraphEff (..)
  , declare
  -- Scope Manipulation
  , currentScope
  , insertEdge
  , newReference
  , newScope
  , withScope
  , declareFunction
  , declareMaybeName
  , reference
  , Has
  ) where

import           Analysis.Name (Name)
import qualified Analysis.Name as Name
import           Control.Algebra
import           Control.Effect.Fresh
import           Control.Effect.Reader
import           Control.Lens
import           Data.List.NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Module as Module
import           Data.ScopeGraph (CurrentScope (..))
import qualified Data.ScopeGraph as ScopeGraph
import           Data.Semilattice.Lower
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Generics (Generic, Generic1)
import           GHC.Records
import qualified Scope.Reference as Reference
import           Source.Span

import qualified Scope.Graph.AdjacencyList as AdjacencyList
import qualified Scope.Path as Scope

import qualified Control.Effect.ScopeGraph.Properties.Declaration as Props
import qualified Control.Effect.ScopeGraph.Properties.Function as Props
import qualified Control.Effect.ScopeGraph.Properties.Reference as Props
import qualified Control.Effect.ScopeGraph.Properties.Reference as Props.Reference
import           Control.Effect.State

-- | Extract the 'Just' of a 'Maybe' in an 'Applicative' context or, given 'Nothing', run the provided action.
maybeM :: Applicative f => f a -> Maybe a -> f a
maybeM f = maybe f pure
{-# INLINE maybeM #-}

type ScopeGraph
  = ScopeGraphEff
  :+: Reader (CurrentScope Name)
  :+: Fresh
  :+: Reader Name

data ScopeGraphEff m k =
    Declare Name Props.Declaration (() -> m k)
  | Reference Text Text Props.Reference (() -> m k)
  | NewScope (Map ScopeGraph.EdgeLabel [Name]) (Name -> m k)
  | InsertEdge ScopeGraph.EdgeLabel (NonEmpty Name) (() -> m k)
  deriving (Generic, Generic1, HFunctor, Effect)

declare :: forall sig m . (Has ScopeGraph sig m) => Name -> Props.Declaration -> m ()
declare n props = send (Declare n props pure)

-- | Establish a reference to a prior declaration.
reference :: forall sig m . (Has ScopeGraph sig m) => Text -> Text -> Props.Reference -> m ()
reference n decl props = send (Reference n decl props pure)

newScope :: forall sig m . Has ScopeGraph sig m => Map ScopeGraph.EdgeLabel [Name] -> m Name
newScope edges = send (NewScope edges pure)

-- | Takes an edge label and a list of names and inserts an import edge to a hole.
newEdge :: Has ScopeGraph sig m => ScopeGraph.EdgeLabel -> NonEmpty Name -> m ()
newEdge label targets = send (InsertEdge label targets pure)


currentScope :: (Has ScopeGraph sig m) => m Name
currentScope = asks unCurrentScope

lookupScope :: Has (State (ScopeGraph.ScopeGraph Name)) sig m => Name -> m (ScopeGraph.Scope Name)
lookupScope address = maybeM undefined . ScopeGraph.lookupScope address =<< get

-- | Inserts a reference.
newReference :: (Has (State (ScopeGraph.ScopeGraph Name)) sig m, Has ScopeGraph sig m) => Name -> Props.Reference -> m ()
newReference name props = do
  currentAddress <- currentScope
  scope <- lookupScope currentAddress

  let refProps = Reference.ReferenceInfo (props^.span_) (Props.Reference.kind props) lowerBound
      insertRef' :: ScopeGraph.Path Name -> ScopeGraph.ScopeGraph Name -> ScopeGraph.ScopeGraph Name
      insertRef' path scopeGraph = let
          scope' = (ScopeGraph.insertReference (Reference.Reference name) lowerBound (Props.Reference.span props) (getField @"kind" props) path) scope
        in
          (ScopeGraph.insertScope currentAddress scope' scopeGraph)
  scopeGraph <- get @(ScopeGraph.ScopeGraph Name)
  case ((AdjacencyList.findPath (const Nothing) (ScopeGraph.Declaration name) currentAddress scopeGraph) :: Maybe (Scope.Path Name)) of
    Just path -> modify (\scopeGraph -> insertRef' path scopeGraph)
    Nothing   -> undefined
  -- maybe
  --   (modify (const (ScopeGraph.insertScope currentAddress (ScopeGraph.newReference (Reference.Reference name) refProps scope))))



declareFunction :: forall sig m . (Has ScopeGraph sig m) => Maybe Name -> Props.Function -> m (Name, Name)
declareFunction name (Props.Function kind span) = do
  currentScope' <- currentScope
  let lexicalEdges = Map.singleton ScopeGraph.Lexical [ currentScope' ]
  associatedScope <- newScope lexicalEdges
  name' <- declareMaybeName name Props.Declaration
                                   { Props.relation = ScopeGraph.Default
                                   , Props.kind = kind
                                   , Props.associatedScope = Just associatedScope
                                   , Props.span = span
                                   }
  pure (name', associatedScope)

declareMaybeName :: Has ScopeGraph sig m
                 => Maybe Name
                 -> Props.Declaration
                 -> m Name
declareMaybeName maybeName props = do
  case maybeName of
    Just name -> name <$ declare name props
    _         -> do
      name <- Name.gensym
      name <$ declare name (props { Props.relation = ScopeGraph.Gensym })

withScope :: Has ScopeGraph sig m
          => Name
          -> m a
          -> m a
withScope scope = local (const scope)
