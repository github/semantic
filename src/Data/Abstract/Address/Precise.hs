{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Address.Precise
( Precise(..)
) where

import Control.Abstract
import Control.Effect.Carrier
import Control.Effect.Sum
import qualified Data.Set as Set
import Prologue

-- | 'Precise' models precise store semantics where only the 'Latest' value is taken. Everything gets it's own address (always makes a new allocation) which makes for a larger store.
newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, NFData)

instance Show Precise where
  showsPrec d = showsUnaryWith showsPrec "Precise" d . unPrecise


instance (Member Fresh sig, Carrier sig m, Monad m) => Carrier (Allocator Precise :+: sig) (AllocatorC Precise m) where
  ret = AllocatorC . ret
  eff = AllocatorC . handleSum
    (eff . handleCoercible)
    (\ (Alloc _ k) -> Precise <$> fresh >>= runAllocatorC . k)


instance Carrier sig m => Carrier (Deref value :+: sig) (DerefC Precise value m) where
  ret = DerefC . ret
  eff = DerefC . handleSum (eff . handleCoercible) (\case
    DerefCell        cell k -> runDerefC (k (fst <$> Set.minView cell))
    AssignCell value _    k -> runDerefC (k (Set.singleton value)))
