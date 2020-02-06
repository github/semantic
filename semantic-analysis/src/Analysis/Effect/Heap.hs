{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts #-}
module Analysis.Effect.Heap
( -- * Heap effect
  deref
, assign
, Heap(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import GHC.Generics (Generic1)

deref :: Has (Heap addr value) sig m => addr -> m (Maybe value)
deref addr = send (Deref addr pure)

assign :: Has (Heap addr value) sig m => addr -> value -> m ()
assign addr value = send (Assign addr value (pure ()))


data Heap addr value m k
  = Deref addr (Maybe value -> m k)
  | Assign addr value (m k)
  deriving (Functor, Generic1)

instance HFunctor (Heap addr value)
instance Effect   (Heap addr value)
