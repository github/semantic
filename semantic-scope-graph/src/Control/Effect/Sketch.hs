{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.Effect.Sketch
  ( Sketch (..)
  , DeclProperties (..)
  , RefProperties (..)
  , declare
  , reference
  , Has
  ) where

import Control.Algebra
--import Data.ScopeGraph (ScopeGraph)
import Data.Text (Text)
import GHC.Generics

data DeclProperties = DeclProperties

data RefProperties = RefProperties

data Sketch address m k =
    Declare Text DeclProperties (() -> m k)
  | Reference Text Text RefProperties (() -> m k)
  deriving (Generic, Generic1, HFunctor, Effect)

declare :: forall a sig m . (Has (Sketch a) sig m) => Text -> DeclProperties -> m ()
declare n props = send @(Sketch a) (Declare n props pure)

reference :: forall a sig m . (Has (Sketch a) sig m) => Text -> Text -> RefProperties -> m ()
reference n decl props = send @(Sketch a) (Reference n decl props pure)
