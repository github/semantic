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


demote :: Evaluator term (Located address) value m a -> Evaluator term address value m a
demote = Evaluator . runEvaluator

promote :: Evaluator term address value m a -> Evaluator term (Located address) value m a
promote = Evaluator . runEvaluator


instance ( Carrier (Allocator address :+: sig) (AllocatorC (Evaluator term address value m))
         , Carrier sig m
         , Member (Reader ModuleInfo) sig
         , Member (Reader PackageInfo) sig
         , Member (Reader Span) sig
         )
      => Carrier (Allocator (Located address) :+: sig) (AllocatorC (Evaluator term (Located address) value m)) where
  ret = AllocatorC . promote . ret
  eff = AllocatorC . (alg \/ (eff . handlePure runAllocatorC))
    where alg (Alloc name k) = promote (Located <$> runAllocatorC (eff (L (Alloc name ret))) <*> currentPackage <*> currentModule <*> pure name <*> ask >>= demote . runAllocatorC . k)


instance (Carrier (Deref value :+: sig) (DerefC (Evaluator term address value m)), Carrier sig m)
      => Carrier (Deref value :+: sig) (DerefC (Evaluator term (Located address) value m)) where
  ret = DerefC . promote . ret
  eff = DerefC . (alg \/ (eff . handlePure runDerefC))
    where alg (DerefCell cell k) = promote (runDerefC (eff (L (DerefCell cell ret))) >>= demote . runDerefC . k)
          alg (AssignCell value cell k) = promote (runDerefC (eff (L (AssignCell value cell ret))) >>= demote . runDerefC . k)
