{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Analysis.Carrier.Heap.Monovariant
( -- * Heap carrier
  HeapC(..)
  -- * Heap effect
, module Analysis.Effect.Heap
) where

import Analysis.Effect.Heap
import Control.Applicative (Alternative)
import Control.Effect.Carrier
import Control.Effect.State.Strict
import Control.Monad ((>=>))
import qualified Control.Monad.Fail as Fail
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Data.Monoid (Alt(..))
import qualified Data.Set as Set

newtype HeapC addr value m a = HeapC { runHeap :: m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail)

instance ( Alternative m
         , Carrier sig m
         , Member (State (Map.Map addr (Set.Set value))) sig
         , Ord addr
         , Ord value
         )
      => Carrier (Heap addr value :+: sig) (HeapC addr value m) where
  eff (L (Deref  addr       k)) = gets (Map.lookup addr >=> nonEmpty . Set.toList) >>= maybe (pure Nothing) (getAlt . foldMap (Alt . pure . Just)) >>= k
  eff (L (Assign addr value k)) = modify (Map.insertWith (<>) addr (Set.singleton value)) >> k
  eff (R other)                 = HeapC (eff (handleCoercible other))
