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
import           Control.Effect.Sketch
import           Control.Monad.IO.Class
import           Data.ScopeGraph (ScopeGraph)
import qualified Data.ScopeGraph as ScopeGraph

newtype SketchC address m a = SketchC (StateC (ScopeGraph address) (FreshC m) a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance forall address sig m . (Effect sig, Algebra sig m) => Algebra (Sketch address :+: sig) (SketchC address m) where
  alg (L (Declare name props k)) = do
    uniq <- SketchC fresh
    let newGraph = ScopeGraph.vertex (ScopeGraph.Decl uniq name)
    SketchC (modify @(ScopeGraph address) (ScopeGraph.addEdge newGraph))
    k
  alg (R other) = SketchC (alg (R (R (handleCoercible other))))

runSketch :: (Functor m) => SketchC address m a -> m (ScopeGraph address, a)
runSketch (SketchC go) = evalFresh 0 . runState ScopeGraph.empty $ go

