{-# LANGUAGE LambdaCase, TypeOperators, UndecidableInstances #-}
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


promoteA :: AllocatorC address m a -> AllocatorC (Hole context address) m a
promoteA = AllocatorC . runAllocatorC

instance ( Carrier (Allocator address :+: sig) (AllocatorC address m)
         , Carrier sig m
         , Monad m
         )
      => Carrier (Allocator (Hole context address) :+: sig) (AllocatorC (Hole context address) m) where
  ret = promoteA . ret
  eff = handleSum
    (AllocatorC . eff . handleCoercible)
    (\ (Alloc name k) -> Total <$> promoteA (eff (L (Alloc name ret))) >>= k)


promoteD :: DerefC address value m a -> DerefC (Hole context address) value m a
promoteD = DerefC . runDerefC

instance (Carrier (Deref value :+: sig) (DerefC address value m), Carrier sig m, Monad m)
      => Carrier (Deref value :+: sig) (DerefC (Hole context address) value m) where
  ret = promoteD . ret
  eff = handleSum (DerefC . eff . handleCoercible) (\case
    DerefCell        cell k -> promoteD (eff (L (DerefCell        cell ret))) >>= k
    AssignCell value cell k -> promoteD (eff (L (AssignCell value cell ret))) >>= k)
