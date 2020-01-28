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

-- | This carrier interprets the Sketch effect, keeping track of
-- the current scope and in-progress graph internally.
module Control.Carrier.Sketch.Fresh
  ( SketchC (..)
  , runSketch
  , module Control.Effect.Sketch
  ) where

import           Analysis.Name (Name)
import qualified Analysis.Name
import           Control.Algebra
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.State.Strict
import           Control.Effect.Sketch
import           Control.Monad.IO.Class
import           Data.Bifunctor
import           Data.Module
import           Data.ScopeGraph (ScopeGraph)
import qualified Data.ScopeGraph as ScopeGraph
import           Data.Semilattice.Lower
import           Source.Span
import qualified System.Path as Path

-- | The state type used to keep track of the in-progress graph and
-- positional/contextual information. The name "sketchbook" is meant
-- to invoke an in-progress, concealed work, as well as the
-- "sketching" of a graph.
data Sketchbook address = Sketchbook
  { sGraph        :: ScopeGraph address
  , sCurrentScope :: address
  } deriving (Eq, Show)

instance Lower (Sketchbook Name) where
  lowerBound =
    let
      initialGraph = ScopeGraph.insertScope n initialScope lowerBound
      n = Data.Name.nameI 0
    in
      Sketchbook initialGraph n

newtype SketchC address m a = SketchC (StateC (Sketchbook address) (FreshC m) a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Effect sig, Algebra sig m) => Algebra (Sketch Name :+: sig) (SketchC Name m) where
  alg (L (Declare n _props k)) = do
    Sketchbook old current <- SketchC (get @(Sketchbook Name))
    let (new, _pos) =
          ScopeGraph.declare
          (ScopeGraph.Declaration (Analysis.Name.name n))
          (lowerBound @ModuleInfo)
          ScopeGraph.Default
          ScopeGraph.Public
          (lowerBound @Span)
          ScopeGraph.Identifier
          Nothing
          current
          old
    SketchC (put @(Sketchbook Name) (Sketchbook new current))
    k ()
  alg (L (Reference n decl _props k)) = do
    Sketchbook old current <- SketchC (get @(Sketchbook Name))
    let new =
          ScopeGraph.reference
          (ScopeGraph.Reference (Analysis.Name.name n))
          (lowerBound @ModuleInfo)
          (lowerBound @Span)
          ScopeGraph.Identifier
          (ScopeGraph.Declaration (Analysis.Name.name decl))
          current
          old
    SketchC (put @(Sketchbook Name) (Sketchbook new current))
    k ()
  alg (R other) = SketchC (alg (R (R (handleCoercible other))))

runSketch ::
  (Functor m)
  => Maybe Path.AbsRelFile
  -> SketchC Name m a
  -> m (ScopeGraph Name, a)
runSketch _rootpath (SketchC go)
  = evalFresh 0
  . fmap (first sGraph)
  . runState lowerBound
  $ go
