{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Analysis.Carrier.Heap.Monovariant
( -- * Heap carrier
  HeapC(..)
  -- * Heap effect
, module Analysis.Effect.Heap
) where

import Analysis.Effect.Heap
import Control.Applicative (Alternative)
import Control.Algebra
import Control.Effect.State
import Control.Monad ((>=>))
import qualified Control.Monad.Fail as Fail
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Data.Monoid (Alt(..))
import qualified Data.Set as Set

newtype HeapC addr value m a = HeapC { runHeap :: m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail)

instance ( Alternative m
         , Has (State (Map.Map addr (Set.Set value))) sig m
         , Ord addr
         , Ord value
         )
      => Algebra (Heap addr value :+: sig) (HeapC addr value m) where
  alg (L (Deref  addr       k)) = gets (Map.lookup addr >=> nonEmpty . Set.toList) >>= maybe (pure Nothing) (getAlt . foldMap (Alt . pure . Just)) >>= k
  alg (L (Assign addr value k)) = modify (Map.insertWith (<>) addr (Set.singleton value)) >> k
  alg (R other)                 = HeapC (alg (handleCoercible other))
