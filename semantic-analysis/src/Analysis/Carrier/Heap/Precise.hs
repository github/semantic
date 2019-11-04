{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Analysis.Carrier.Heap.Precise
( -- * Heap carrier
  runHeap
, HeapC(..)
  -- * Heap effect
, module Analysis.Effect.Heap
) where

import Analysis.Effect.Heap
import Control.Effect.Carrier
import Control.Effect.State.Strict
import qualified Control.Monad.Fail as Fail
import qualified Data.IntMap as IntMap

type Precise = Int

runHeap :: HeapC value m a -> m (IntMap.IntMap value, a)
runHeap (HeapC m) = runState mempty m

newtype HeapC value m a = HeapC (StateC (IntMap.IntMap value) m a)
  deriving (Applicative, Functor, Monad, Fail.MonadFail)

instance (Carrier sig m, Effect sig)
      => Carrier (Heap Precise value :+: sig) (HeapC value m) where
  eff (L (Deref addr k))        = HeapC (gets (IntMap.lookup addr)) >>= k
  eff (L (Assign addr value k)) = HeapC (modify (IntMap.insert addr value)) >> k
  eff (R other)                 = HeapC (eff (R (handleCoercible other)))
