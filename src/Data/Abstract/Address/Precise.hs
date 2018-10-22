{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Address.Precise
( Precise(..)
) where

import Control.Abstract
import qualified Data.Set as Set
import Prologue

-- | 'Precise' models precise store semantics where only the 'Latest' value is taken. Everything gets it's own address (always makes a new allocation) which makes for a larger store.
newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, NFData)

instance Show Precise where
  showsPrec d = showsUnaryWith showsPrec "Precise" d . unPrecise


instance (Member Fresh sig, Carrier sig m) => Carrier (Allocator Precise :+: sig) (AllocatorC (Evaluator term Precise value m)) where
  gen = AllocatorC . gen
  alg = AllocatorC . (algA \/ (alg . handlePure runAllocatorC))
    where algA (Alloc _ k) = Precise <$> fresh >>= runAllocatorC . k


instance Carrier sig m => Carrier (Deref value :+: sig) (DerefC (Evaluator term Precise value m)) where
  gen = DerefC . gen
  alg = DerefC . (algD \/ (alg . handlePure runDerefC))
    where algD (DerefCell cell k) = runDerefC (k (fst <$> Set.minView cell))
          algD (AssignCell value _ k) = runDerefC (k (Set.singleton value))
