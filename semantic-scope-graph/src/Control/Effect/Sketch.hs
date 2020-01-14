{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.Effect.Sketch
  ( Sketch (..)
  , DeclProperties (..)
  , declare
  , Has
  ) where

import Control.Algebra
--import Data.ScopeGraph (ScopeGraph)
import Data.Text (Text)
import GHC.Generics

data DeclProperties = DeclProperties

data Sketch address m k =
  Declare Text DeclProperties (() -> m k)
  deriving (Generic, Generic1, HFunctor, Effect)

declare :: forall a sig m . (Has (Sketch a) sig m) => Text -> DeclProperties -> m ()
declare n props = send @(Sketch a) (Declare n props pure)
