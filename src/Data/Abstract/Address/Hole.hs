{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Abstract.Address.Hole
( Hole(..)
, toMaybe
) where

import Control.Abstract
import Control.Algebra
import Data.Semilattice.Lower

data Hole context a = Partial context | Total a
  deriving (Foldable, Functor, Eq, Ord, Show, Traversable)

instance Lower context => AbstractHole (Hole context a) where
  hole = Partial lowerBound

toMaybe :: Hole context a -> Maybe a
toMaybe (Partial _) = Nothing
toMaybe (Total a)   = Just a


promoteA :: AllocatorC address m a -> AllocatorC (Hole context address) m a
promoteA = AllocatorC . runAllocatorC

instance ( Algebra (Allocator address :+: sig) (AllocatorC address m)
         , Algebra sig m
         , Monad m
         )
      => Algebra (Allocator (Hole context address) :+: sig) (AllocatorC (Hole context address) m) where
  alg (R other)          = AllocatorC . alg . handleCoercible $ other
  alg (L (Alloc name k)) = Total <$> promoteA (alg (L (Alloc name pure))) >>= k


promoteD :: DerefC address value m a -> DerefC (Hole context address) value m a
promoteD = DerefC . runDerefC

instance (Algebra (Deref value :+: sig) (DerefC address value m), Algebra sig m)
      => Algebra (Deref value :+: sig) (DerefC (Hole context address) value m) where
  alg (R other) = DerefC . alg . handleCoercible $ other
  alg (L op) = case op of
    DerefCell        cell k -> promoteD (alg (L (DerefCell        cell pure))) >>= k
    AssignCell value cell k -> promoteD (alg (L (AssignCell value cell pure))) >>= k
