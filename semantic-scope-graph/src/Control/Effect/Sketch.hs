{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The Sketch effect is used to build up a scope graph over
-- the lifetime of a monadic computation. The name is meant to evoke
-- physically sketching the hierarchical outline of a graph.
module Control.Effect.Sketch
  ( Sketch
  , SketchEff (..)
  , DeclProperties (..)
  , RefProperties (..)
  , FunProperties (..)
  , declare
  -- Scope Manipulation
  , currentScope
  , insertEdge
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
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.ScopeGraph as ScopeGraph
import           Data.Text (Text)
import           GHC.Generics (Generic, Generic1)
import           GHC.Records
import Data.List.NonEmpty

data DeclProperties = DeclProperties {
    kind            :: ScopeGraph.Kind
  , relation        :: ScopeGraph.Relation
  , associatedScope :: Maybe Name
}

data RefProperties = RefProperties
data FunProperties = FunProperties {
  kind :: ScopeGraph.Kind
}

type Sketch
  = SketchEff
  :+: Fresh
  :+: Reader Name

data SketchEff m k =
    Declare Name DeclProperties (() -> m k)
  | Reference Text Text RefProperties (() -> m k)
  | NewScope (Map ScopeGraph.EdgeLabel [Name]) (Name -> m k)
  | InsertEdge ScopeGraph.EdgeLabel (NonEmpty Name) (() -> m k)
  deriving (Generic, Generic1, HFunctor, Effect)

currentScope :: Has (Reader Name) sig m => m Name
currentScope = ask

declare :: forall sig m . (Has Sketch sig m) => Name -> DeclProperties -> m ()
declare n props = send (Declare n props pure)

-- | Establish a reference to a prior declaration.
reference :: forall sig m . (Has Sketch sig m) => Text -> Text -> RefProperties -> m ()
reference n decl props = send (Reference n decl props pure)

newScope :: forall sig m . (Has Sketch sig m) => Map ScopeGraph.EdgeLabel [Name] -> m Name
newScope edges = send (NewScope edges pure)

insertEdge :: Has Sketch sig m => ScopeGraph.EdgeLabel -> NonEmpty Name -> m ()
insertEdge label targets = send (InsertEdge label targets pure)

declareFunction :: forall sig m . (Has Sketch sig m) => Maybe Name -> FunProperties -> m (Name, Name)
declareFunction name props = do
  currentScope' <- currentScope
  let lexicalEdges = Map.singleton ScopeGraph.Lexical [ currentScope' ]
  associatedScope <- newScope lexicalEdges
  name' <- declareMaybeName name (DeclProperties { relation = ScopeGraph.Default, kind = (getField @"kind" @FunProperties props), associatedScope = Just associatedScope })
  pure (name', associatedScope)

declareMaybeName :: Has Sketch sig m
                 => Maybe Name
                 -> DeclProperties
                 -> m Name
declareMaybeName maybeName props = do
  case maybeName of
    Just name -> declare name props >> pure name
    _         -> Name.gensym >>= \name -> declare name (props { relation = ScopeGraph.Gensym }) >> pure name -- TODO: Modify props and change Kind to Gensym

withScope :: Has Sketch sig m
          => Name
          -> m a
          -> m a
withScope scope = local (const scope)
