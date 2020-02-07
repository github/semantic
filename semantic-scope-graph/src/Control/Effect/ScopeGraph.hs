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
import           Data.List.NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.ScopeGraph as ScopeGraph
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Generics (Generic, Generic1)

import qualified Control.Effect.ScopeGraph.Properties.Declaration as Props
import qualified Control.Effect.ScopeGraph.Properties.Function as Props
import qualified Control.Effect.ScopeGraph.Properties.Reference as Props

type ScopeGraph
  = ScopeGraphEff
  :+: Fresh
  :+: Reader Name

data ScopeGraphEff m k =
    Declare Name Props.Declaration (() -> m k)
  | Reference Text Text Props.Reference (() -> m k)
  | NewScope (Map ScopeGraph.EdgeLabel [Name]) (Name -> m k)
  | InsertEdge ScopeGraph.EdgeLabel (NonEmpty Name) (() -> m k)
  deriving (Generic, Generic1, HFunctor, Effect)

currentScope :: Has (Reader Name) sig m => m Name
currentScope = ask

declare :: forall sig m . (Has ScopeGraph sig m) => Name -> Props.Declaration -> m ()
declare n props = send (Declare n props pure)

-- | Establish a reference to a prior declaration.
reference :: forall sig m . (Has ScopeGraph sig m) => Text -> Text -> Props.Reference -> m ()
reference n decl props = send (Reference n decl props pure)

newScope :: forall sig m . Has ScopeGraph sig m => Map ScopeGraph.EdgeLabel [Name] -> m Name
newScope edges = send (NewScope edges pure)

-- | Takes an edge label and a list of names and inserts an import edge to a hole.
insertEdge :: Has ScopeGraph sig m => ScopeGraph.EdgeLabel -> NonEmpty Name -> m ()
insertEdge label targets = send (InsertEdge label targets pure)

-- | Inserts a reference.
newReference :: Has ScopeGraph sig m => Name -> Props.Reference -> m ()
newReference name targets = reference (Text.pack $ show name) (Text.pack $ show name) targets


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
