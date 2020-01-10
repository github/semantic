{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Carrier.Sketch.Fresh
  ( SketchC (..)
  , runSketch
  , module Control.Effect.Sketch
  ) where

import           Control.Algebra
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.State.Strict
import Data.Bifunctor
import           Control.Effect.Sketch
import           Control.Monad.IO.Class
import           Data.ScopeGraph (ScopeGraph)
import qualified Data.ScopeGraph as ScopeGraph{-# LANGUAGE  #-}

data Sketchbook address = Sketchbook
  { sGraph  :: ScopeGraph address
  , sParent :: ScopeGraph.Vertex (ScopeGraph address)
  }

newtype SketchC address m a = SketchC (StateC (Sketchbook address) (FreshC m) a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance forall address sig m . (ScopeGraph.Addressable address, Effect sig, Algebra sig m) => Algebra (Sketch address :+: sig) (SketchC address m) where
  alg (L (Declare name _props k)) = do
    ua <- SketchC fresh
    ub <- SketchC fresh
    parent <- SketchC (gets sParent)
    let newScope = ScopeGraph.scope ua
    let newDecl  = ScopeGraph.decl ub name
    let newGraph = ScopeGraph.overlays [ ScopeGraph.edge parent newScope
                                        , ScopeGraph.edge newScope newDecl
                                        ]
    SketchC (modify @(Sketchbook address) (\(Sketchbook g _) -> Sketchbook (ScopeGraph.overlay newGraph g) newScope))
    k
  alg (R other) = SketchC (alg (R (R (handleCoercible other))))

runSketch :: (ScopeGraph.Addressable address, Functor m) => SketchC address m a -> m (ScopeGraph address, a)
runSketch (SketchC go) = evalFresh 0 . fmap (first sGraph) . runState (Sketchbook ScopeGraph.empty ScopeGraph.root) $ go

