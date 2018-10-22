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


demote :: Evaluator term (Hole context address) value m a -> Evaluator term address value m a
demote = Evaluator . runEvaluator

promote :: Evaluator term address value m a -> Evaluator term (Hole context address) value m a
promote = Evaluator . runEvaluator


instance ( Carrier (Allocator address :+: sig) (AllocatorC (Evaluator term address value m))
         , Carrier sig m
         )
      => Carrier (Allocator (Hole context address) :+: sig) (AllocatorC (Evaluator term (Hole context address) value m)) where
  ret = AllocatorC . promote . ret
  eff = AllocatorC . (alg \/ (eff . handlePure runAllocatorC))
    where alg (Alloc name k) = promote (Total <$> runAllocatorC (eff (L (Alloc name ret))) >>= demote . runAllocatorC . k)


instance (Carrier (Deref value :+: sig) (DerefC (Evaluator term address value m)), Carrier sig m)
      => Carrier (Deref value :+: sig) (DerefC (Evaluator term (Hole context address) value m)) where
  ret = DerefC . promote . ret
  eff = DerefC . (alg \/ (eff . handlePure runDerefC))
    where alg (DerefCell cell k) = promote (runDerefC (eff (L (DerefCell cell ret))) >>= demote . runDerefC . k)
          alg (AssignCell value cell k) = promote (runDerefC (eff (L (AssignCell value cell ret))) >>= demote . runDerefC . k)
