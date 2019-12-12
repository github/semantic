{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Analysis.Carrier.Heap.Precise
( -- * Heap carrier
  runHeap
, HeapC(..)
  -- * Heap effect
, module Analysis.Effect.Heap
) where

import Analysis.Effect.Heap
import Control.Algebra
import Control.Carrier.State.Strict
import qualified Control.Monad.Fail as Fail
import qualified Data.IntMap as IntMap

type Precise = Int

runHeap :: HeapC value m a -> m (IntMap.IntMap value, a)
runHeap (HeapC m) = runState mempty m

newtype HeapC value m a = HeapC (StateC (IntMap.IntMap value) m a)
  deriving (Applicative, Functor, Monad, Fail.MonadFail)

instance (Algebra sig m, Effect sig)
      => Algebra (Heap Precise value :+: State (IntMap.IntMap value) :+: sig) (HeapC value m) where
  alg (L (Deref addr k))        = HeapC (gets (IntMap.lookup addr)) >>= k
  alg (L (Assign addr value k)) = HeapC (modify (IntMap.insert addr value)) >> k
  alg (R other)                 = HeapC (alg (handleCoercible other))
