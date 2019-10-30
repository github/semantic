{-# LANGUAGE DeriveTraversable, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Address.Hole
( Hole(..)
, toMaybe
) where

import Control.Abstract
import Control.Effect.Carrier
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
  eff (R other) = AllocatorC . eff . handleCoercible $ other
  eff (L (Alloc name k)) = Total <$> promoteA (eff (L (Alloc name pure))) >>= k


promoteD :: DerefC address value m a -> DerefC (Hole context address) value m a
promoteD = DerefC . runDerefC

instance (Carrier (Deref value :+: sig) (DerefC address value m), Carrier sig m)
      => Carrier (Deref value :+: sig) (DerefC (Hole context address) value m) where
  eff (R other) = DerefC . eff . handleCoercible $ other
  eff (L op) = case op of
    DerefCell        cell k -> promoteD (eff (L (DerefCell        cell pure))) >>= k
    AssignCell value cell k -> promoteD (eff (L (AssignCell value cell pure))) >>= k
