{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
import Data.Kind (Type)

deref :: Has (Heap addr value) sig m => addr -> m (Maybe value)
deref addr = send (Deref addr)

assign :: Has (Heap addr value) sig m => addr -> value -> m ()
assign addr value = send (Assign addr value)


data Heap addr value (m :: Type -> Type) k where
  Deref :: addr -> Heap addr value m (Maybe value)
  Assign :: addr -> value -> Heap addr value m ()
