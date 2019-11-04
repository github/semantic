{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Analysis.Carrier.Heap.Precise
( -- * Heap carrier
  runHeap
, HeapC(..)
  -- * Heap effect
, module Analysis.Effect.Heap
) where

import Analysis.Effect.Heap
import Control.Effect.State.Strict
import qualified Control.Monad.Fail as Fail
import qualified Data.IntMap as IntMap

runHeap :: HeapC value m a -> m (IntMap.IntMap value, a)
runHeap (HeapC m) = runState mempty m

newtype HeapC value m a = HeapC (StateC (IntMap.IntMap value) m a)
  deriving (Applicative, Functor, Monad, Fail.MonadFail)
