{-# LANGUAGE TypeOperators, UndecidableInstances #-}
module Data.Abstract.Address.Hole
( Hole(..)
, toMaybe
) where

import Control.Abstract
import Control.Effect.Carrier
import Control.Effect.Sum
import Prologue

data Hole context a = Partial context | Total a
  deriving (Foldable, Functor, Eq, Ord, Show, Traversable)

instance Lower context => AbstractHole (Hole context a) where
  hole = Partial lowerBound

toMaybe :: Hole context a -> Maybe a
toMaybe (Partial _) = Nothing
toMaybe (Total a)   = Just a


demoteD :: DerefC (Hole context address) value m a -> DerefC address value m a
demoteD = DerefC . runDerefC

promoteD :: DerefC address value m a -> DerefC (Hole context address) value m a
promoteD = DerefC . runDerefC


demoteA :: AllocatorC (Hole context address) m a -> AllocatorC address m a
demoteA = AllocatorC . runAllocatorC

promoteA :: AllocatorC address m address -> AllocatorC (Hole context address) m address
promoteA = AllocatorC . runAllocatorC


instance ( Carrier (Allocator address :+: sig) (AllocatorC address m)
         , Carrier sig m
         , Monad m
         )
      => Carrier (Allocator (Hole context address) :+: sig) (AllocatorC (Hole context address) m) where
  ret = demoteA . ret
  eff = alg \/ AllocatorC . eff . handleCoercible
    where alg (Alloc name k) = Total <$> promoteA (eff (L (Alloc name ret))) >>= k


instance (Carrier (Deref value :+: sig) (DerefC address value m), Carrier sig m, Monad m)
      => Carrier (Deref value :+: sig) (DerefC (Hole context address) value m) where
  ret = demoteD . ret
  eff = alg \/ DerefC . eff . handleCoercible
    where alg (DerefCell cell k) = promoteD (eff (L (DerefCell cell ret))) >>= k
          alg (AssignCell value cell k) = promoteD (eff (L (AssignCell value cell ret))) >>= k
