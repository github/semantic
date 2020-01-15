{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The Sketch effect is used to build up a scope graph over
-- the lifetime of a monadic computation. The name is meant to evoke
-- physically sketching the hierarchical outline of a graph.
module Control.Effect.Sketch
  ( Sketch (..)
  , DeclProperties (..)
  , RefProperties (..)
  , declare
  , reference
  , Has
  ) where

import Control.Algebra
import Data.Text (Text)
import GHC.Generics

data DeclProperties = DeclProperties

data RefProperties = RefProperties

data Sketch address m k =
    Declare Text DeclProperties (() -> m k)
  | Reference Text Text RefProperties (() -> m k)
  deriving (Generic, Generic1, HFunctor, Effect)

-- | Introduces a declaration into the scope.
declare :: forall a sig m . (Has (Sketch a) sig m) => Text -> DeclProperties -> m ()
declare n props = send @(Sketch a) (Declare n props pure)

-- | Establish a reference to a prior declaration.
reference :: forall a sig m . (Has (Sketch a) sig m) => Text -> Text -> RefProperties -> m ()
reference n decl props = send @(Sketch a) (Reference n decl props pure)
