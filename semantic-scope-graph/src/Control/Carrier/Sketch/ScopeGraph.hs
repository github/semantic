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

-- | This carrier interprets the Sketch effect, keeping track of
-- the current scope and in-progress graph internally.
module Control.Carrier.Sketch.ScopeGraph
  ( SketchC (..)
  , runSketch
  , module Control.Effect.ScopeGraph
  ) where

import           Analysis.Name (Name)
import qualified Analysis.Name as Name
import           Control.Algebra
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.ScopeGraph (ScopeGraphEff (..))
import           Control.Monad.IO.Class
import           Data.Bifunctor
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Module
import           Data.ScopeGraph (ScopeGraph)
import qualified Data.ScopeGraph as ScopeGraph
import           Data.Semilattice.Lower
import qualified ScopeGraph.Properties.Declaration as Props
import           Source.Span
import qualified System.Path as Path

-- | The state type used to keep track of the in-progress graph and
-- positional/contextual information. The name "sketchbook" is meant
-- to invoke an in-progress, concealed work, as well as the
-- "sketching" of a graph.
data Sketchbook = Sketchbook
  { sGraph        :: ScopeGraph Name
  , sCurrentScope :: Name
  } deriving (Eq, Show)

instance Lower Sketchbook where
  lowerBound =
    let
      initialGraph = ScopeGraph.insertScope n lowerBound lowerBound
      n = Name.nameI 0
    in
      Sketchbook initialGraph n

newtype SketchC address m a = SketchC (StateC Sketchbook (FreshC m) a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Effect sig, Algebra sig m) => Algebra (ScopeGraphEff :+: Reader Name :+: Fresh :+: sig) (SketchC Name m) where
  alg (L (Declare n props k)) = do
    Sketchbook old current <- SketchC (get @Sketchbook)
    let Props.Declaration kind relation associatedScope span = props
    let (new, _pos) =
          ScopeGraph.declare
          (ScopeGraph.Declaration n)
          (lowerBound @ModuleInfo)
          relation
          ScopeGraph.Public
          span
          kind
          associatedScope
          current
          old
    SketchC (put (Sketchbook new current))
    k ()
  alg (L (Reference n decl _props k)) = do
    Sketchbook old current <- SketchC (get @Sketchbook)
    let new =
          ScopeGraph.reference
          (ScopeGraph.Reference (Name.name n))
          (lowerBound @ModuleInfo)
          (lowerBound @Span)
          ScopeGraph.Identifier
          (ScopeGraph.Declaration (Name.name decl))
          current
          old
    SketchC (put (Sketchbook new current))
    k ()
  alg (L (NewScope edges k)) = do
    Sketchbook old current <- SketchC get
    name <- SketchC Name.gensym
    let new = ScopeGraph.newScope name edges old
    SketchC (put (Sketchbook new current))
    k name
  alg (L (InsertEdge label address k)) = do
    Sketchbook old current <- SketchC get
    let new = ScopeGraph.addImportEdge label (NonEmpty.toList address) current old
    SketchC (put (Sketchbook new current))
    k ()

  alg (R (L a)) = case a of
    Ask k -> SketchC (gets sCurrentScope) >>= k
    Local fn go k -> do
      initial@(Sketchbook s oldScope) <- SketchC get
      let newScope = fn oldScope
      SketchC (put (Sketchbook s newScope))
      result <- go
      SketchC (put initial)
      k result

  alg (R (R (L a))) = send (handleCoercible a)
  alg (R (R (R a))) = send (handleCoercible a)

runSketch ::
  (Functor m)
  => Maybe Path.AbsRelFile
  -> SketchC Name m a
  -> m (ScopeGraph Name, a)
runSketch _rootpath (SketchC go)
  = evalFresh 1
  . fmap (first sGraph)
  . runState lowerBound
  $ go
