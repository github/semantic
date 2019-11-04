{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Analysis.Carrier.Heap.Monovariant
( -- * Heap carrier
  runHeap
, HeapC(..)
  -- * Heap effect
, module Analysis.Effect.Heap
) where

import Analysis.Effect.Heap
import Control.Effect.State.Strict
import qualified Control.Monad.Fail as Fail
import qualified Data.Map as Map
import qualified Data.Set as Set

runHeap :: HeapC addr value m a -> m (Map.Map addr (Set.Set value), a)
runHeap (HeapC m) = runState Map.empty m

newtype HeapC addr value m a = HeapC (StateC (Map.Map addr (Set.Set value)) m a)
  deriving (Applicative, Functor, Monad, Fail.MonadFail)
