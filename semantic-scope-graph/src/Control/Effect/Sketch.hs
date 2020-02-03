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
import qualified ScopeGraph.Properties.Declaration as Props
import qualified ScopeGraph.Properties.Function as Props
import qualified ScopeGraph.Properties.Reference as Props

type Sketch
  = SketchEff
  :+: Fresh
  :+: Reader Name

data SketchEff m k =
    Declare Name Props.Declaration (() -> m k)
  | Reference Text Text Props.Reference (() -> m k)
  | NewScope (Map ScopeGraph.EdgeLabel [Name]) (Name -> m k)
  deriving (Generic, Generic1, HFunctor, Effect)

currentScope :: Has (Reader Name) sig m => m Name
currentScope = ask

declare :: forall sig m . (Has Sketch sig m) => Name -> Props.Declaration -> m ()
declare n props = send (Declare n props pure)

-- | Establish a reference to a prior declaration.
reference :: forall sig m . (Has Sketch sig m) => Text -> Text -> Props.Reference -> m ()
reference n decl props = send (Reference n decl props pure)

newScope :: forall sig m . (Has Sketch sig m) => Map ScopeGraph.EdgeLabel [Name] -> m Name
newScope edges = send (NewScope edges pure)

declareFunction :: forall sig m . (Has Sketch sig m) => Maybe Name -> Props.Function -> m (Name, Name)
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

declareMaybeName :: Has Sketch sig m
                 => Maybe Name
                 -> Props.Declaration
                 -> m Name
declareMaybeName maybeName props = do
  case maybeName of
    Just name -> name <$ declare name props
    _         -> do
      name <- Name.gensym
      name <$ declare name (props { Props.relation = ScopeGraph.Gensym })

withScope :: Has Sketch sig m
          => Name
          -> m a
          -> m a
withScope scope = local (const scope)

