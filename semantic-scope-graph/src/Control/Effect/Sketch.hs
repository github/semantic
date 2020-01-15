{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Control.Effect.Sketch
  ( Sketch (..)
  , DeclProperties (..)
  , RefProperties (..)
  , FunProperties (..)
  , declare
  , currentScope
  , newScope
  , declareFunction
  -- , declareFunction
  , reference
  , Has
  ) where

import GHC.Records
import Control.Effect.Fresh
import Control.Algebra
--import Data.ScopeGraph (ScopeGraph)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics
import           Data.Name (Name)
import qualified Data.Name as Name
import qualified Data.ScopeGraph as ScopeGraph

data DeclProperties address = DeclProperties {
    kind :: ScopeGraph.Kind
  , relation :: ScopeGraph.Relation
  , associatedScope :: Maybe address
}

data RefProperties = RefProperties
data FunProperties = FunProperties {
  kind :: ScopeGraph.Kind
}

data Sketch address m k =
    Declare Name (DeclProperties address) (() -> m k)
  | Reference Text Text RefProperties (() -> m k)
  | CurrentScope (address -> m k)
  | NewScope (Map ScopeGraph.EdgeLabel [address]) (address -> m k)
  deriving (Generic, Generic1, HFunctor, Effect)

declare :: forall a sig m . (Has (Sketch a) sig m) => Name -> DeclProperties a -> m ()
declare n props = send @(Sketch a) (Declare n props pure)

reference :: forall a sig m . (Has (Sketch a) sig m) => Text -> Text -> RefProperties -> m ()
reference n decl props = send @(Sketch a) (Reference n decl props pure)

currentScope :: forall address sig m . (Has (Sketch address) sig m) => m address
currentScope = send @(Sketch address) (CurrentScope pure)

newScope :: forall address sig m . (Has (Sketch address) sig m) => Map ScopeGraph.EdgeLabel [address] -> m address
newScope edges = send @(Sketch address) (NewScope edges pure)

declareFunction :: forall address sig m . (Has (Sketch address) sig m, Has Fresh sig m) => Maybe Name -> FunProperties -> m (Name, address)
declareFunction name props = do
  currentScope' <- currentScope
  let lexicalEdges = Map.singleton ScopeGraph.Lexical [ currentScope' ]
  associatedScope <- newScope lexicalEdges
  name' <- declareMaybeName name (DeclProperties { relation = ScopeGraph.Default, kind = (getField @"kind" @FunProperties props), associatedScope = Just associatedScope })
  pure (name', associatedScope)

declareMaybeName :: forall address sig m . (Has (Sketch address) sig m, Has Fresh sig m)
                 => Maybe Name
                 -> DeclProperties address
                 -> m Name
declareMaybeName maybeName props = do
  case maybeName of
    Just name -> declare name props >> pure name
    _         -> Name.gensym >>= \name -> declare name (props { relation = ScopeGraph.Gensym }) >> pure name -- TODO: Modify props and change Kind to Gensym

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
