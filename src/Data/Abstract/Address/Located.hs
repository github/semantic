{-# LANGUAGE TypeOperators, UndecidableInstances #-}
module Data.Abstract.Address.Located
( Located(..)
) where

import Control.Abstract
import Control.Effect.Carrier
import Control.Effect.Sum
import Data.Abstract.Module (ModuleInfo)
import Data.Abstract.Name
import Data.Abstract.Package (PackageInfo)

data Located address = Located
  { address        :: address
  , addressPackage :: {-# UNPACK #-} !PackageInfo
  , addressModule  :: !ModuleInfo
  , addressName    :: Name
  , addressSpan    :: Span
  }
  deriving (Eq, Ord, Show)


demoteD :: DerefC (Located address) value m a -> DerefC address value m a
demoteD = DerefC . runDerefC

promoteD :: DerefC address value m a -> DerefC (Located address) value m a
promoteD = DerefC . runDerefC


demoteA :: AllocatorC (Located address) m a -> AllocatorC address m a
demoteA = AllocatorC . runAllocatorC

promoteA :: AllocatorC address m a -> AllocatorC (Located address) m a
promoteA = AllocatorC . runAllocatorC


instance ( Carrier (Allocator address :+: sig) (AllocatorC address m)
         , Carrier sig m
         , Member (Reader ModuleInfo) sig
         , Member (Reader PackageInfo) sig
         , Member (Reader Span) sig
         , Monad m
         )
      => Carrier (Allocator (Located address) :+: sig) (AllocatorC (Located address) m) where
  ret = promoteA . ret
  eff = alg \/ AllocatorC . eff . handleCoercible
    where alg (Alloc name k) = Located <$> promoteA (eff (L (Alloc name ret))) <*> currentPackage <*> currentModule <*> pure name <*> ask >>= k


instance (Carrier (Deref value :+: sig) (DerefC address value m), Carrier sig m, Monad m)
      => Carrier (Deref value :+: sig) (DerefC (Located address) value m) where
  ret = promoteD . ret
  eff = alg \/ DerefC . eff . handleCoercible
    where alg (DerefCell cell k) = promoteD (eff (L (DerefCell cell ret))) >>= k
          alg (AssignCell value cell k) = promoteD (eff (L (AssignCell value cell ret))) >>= k
