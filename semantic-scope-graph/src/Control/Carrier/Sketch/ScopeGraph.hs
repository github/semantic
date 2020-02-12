{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fprint-expanded-synonyms #-}

-- | This carrier interprets the Sketch effect, keeping track of
-- the current scope and in-progress graph internally.
module Control.Carrier.Sketch.ScopeGraph
  ( SketchC
  , runSketch
  , module Control.Effect.ScopeGraph
  ) where

import           Analysis.Name (Name)
import qualified Analysis.Name as Name
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.ScopeGraph
import           Data.Module (ModuleInfo)
import qualified Data.ScopeGraph as ScopeGraph
import           Data.Semilattice.Lower

type SketchC addr m
  = StateC (ScopeGraph Name)
  ( StateC Name
  ( ReaderC Name
  ( ReaderC ModuleInfo
  ( FreshC m
  ))))

runSketch ::
  (Functor m)
  => ModuleInfo
  -> SketchC Name m a
  -> m (ScopeGraph Name, a)
runSketch info go
  = evalFresh 0
  . runReader @ModuleInfo info
  . runReader @Name rootname
  . evalState @Name rootname
  . runState @(ScopeGraph Name) initialGraph
  $ go
  where
    rootname = Name.nameI 0
    initialGraph = ScopeGraph.insertScope rootname lowerBound lowerBound
