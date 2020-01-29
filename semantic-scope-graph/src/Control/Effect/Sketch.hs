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
  , newScope
  , withScope
  , declareFunction
  , declareMaybeName
  , reference
  , Has
  ) where

import           Control.Algebra
import           Control.Effect.Fresh
import           Control.Effect.Reader
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Analysis.Name (Name)
import qualified Analysis.Name as Name
import qualified Data.ScopeGraph as ScopeGraph
import           Data.Text (Text)
import           GHC.Generics (Generic, Generic1)
import           GHC.Records

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
-- declareFunction :: ( Has (State (ScopeGraph address)) sig m
--                    , Has (Allocator address) sig m
--                    , Has (Reader (CurrentScope address)) sig m
--                    , Has (Reader ModuleInfo) sig m
--                    , Has Fresh sig m
--                    , Ord address
--                    )
--                 => Maybe Name
--                 -> ScopeGraph.AccessControl
--                 -> Span
--                 -> ScopeGraph.Kind
--                 -> Evaluator term address value m (Name, address)
-- declareFunction name accessControl span kind = do
--   currentScope' <- currentScope
--   let lexicalEdges = Map.singleton Lexical [ currentScope' ]
--   associatedScope <- newScope lexicalEdges
--   name' <- declareMaybeName name Default accessControl span kind (Just associatedScope)
--   pure (name', associatedScope)
