{-# LANGUAGE TypeOperators, UndecidableInstances #-}
module Data.Abstract.Address.Hole
( Hole(..)
, toMaybe
) where

import Control.Abstract
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
  gen = AllocatorC . promote . gen
  alg = AllocatorC . (algA \/ (alg . handlePure runAllocatorC))
    where algA (Alloc name k) = promote (Total <$> runAllocatorC (alg (L (Alloc name gen))) >>= demote . runAllocatorC . k)


instance (Carrier (Deref value :+: sig) (DerefC (Evaluator term address value m)), Carrier sig m)
      => Carrier (Deref value :+: sig) (DerefC (Evaluator term (Hole context address) value m)) where
  gen = DerefC . promote . gen
  alg = DerefC . (algD \/ (alg . handlePure runDerefC))
    where algD (DerefCell cell k) = promote (runDerefC (alg (L (DerefCell cell gen))) >>= demote . runDerefC . k)
          algD (AssignCell value cell k) = promote (runDerefC (alg (L (AssignCell value cell gen))) >>= demote . runDerefC . k)
