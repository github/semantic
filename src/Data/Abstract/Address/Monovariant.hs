{-# LANGUAGE GADTs, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Address.Monovariant
( Monovariant(..)
) where

import Control.Abstract
import Data.Abstract.Name
import qualified Data.Set as Set
import Prologue

-- | 'Monovariant' models using one address for a particular name. It trackes the set of values that a particular address takes and uses it's name to lookup in the store and only allocation if new.
newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord)

instance Show Monovariant where
  showsPrec d = showsUnaryWith showsPrec "Monovariant" d . unMonovariant


instance Carrier sig m => Carrier (Allocator Monovariant :+: sig) (AllocatorC (Evaluator term Monovariant value m)) where
  gen = AllocatorC . gen
  alg = AllocatorC . (algA \/ (alg . handlePure runAllocatorC))
    where algA (Alloc name k) = runAllocatorC (k (Monovariant name))


instance (Member NonDet sig, Ord value, Carrier sig m) => Carrier (Deref value :+: sig) (DerefC (Evaluator term Monovariant value m)) where
  gen = DerefC . gen
  alg = DerefC . (algD \/ (alg . handlePure runDerefC))
    where algD (DerefCell cell k) = traverse (foldMapA pure) (nonEmpty (toList cell)) >>= runDerefC . k
          algD (AssignCell value cell k) = runDerefC (k (Set.insert value cell))
