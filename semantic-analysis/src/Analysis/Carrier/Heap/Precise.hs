{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Analysis.Carrier.Heap.Precise
( -- * Heap carrier
  HeapC(..)
  -- * Heap effect
, module Analysis.Effect.Heap
) where

import Analysis.Effect.Heap
import qualified Control.Monad.Fail as Fail

newtype HeapC addr value m a = HeapC { runHeap :: m a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail)
