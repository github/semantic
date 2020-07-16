{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Carrier.Heap.Precise
( -- * Heap carrier
  runHeap
, HeapC(HeapC)
  -- * Heap effect
, module Analysis.Effect.Heap
) where

import           Analysis.Effect.Heap
import           Control.Algebra
import           Control.Carrier.State.Strict
import qualified Control.Monad.Fail as Fail
import qualified Data.IntMap as IntMap

type Precise = Int

runHeap :: HeapC value m a -> m (IntMap.IntMap value, a)
runHeap (HeapC m) = runState mempty m

newtype HeapC value m a = HeapC { runHeapC :: StateC (IntMap.IntMap value) m a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail)

instance Algebra sig m
      => Algebra (Heap Precise value :+: State (IntMap.IntMap value) :+: sig) (HeapC value m) where
  alg hdl sig ctx = HeapC $ case sig of
    L (Deref addr)        -> (<$ ctx) <$> gets (IntMap.lookup addr)
    L (Assign addr value) -> ctx <$ modify (IntMap.insert addr value)
    R other               -> alg (runHeapC . hdl) other ctx
