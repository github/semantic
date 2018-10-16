{-# LANGUAGE TypeOperators, UndecidableInstances #-}
module Data.Abstract.Address.Located
( Located(..)
) where

import Control.Abstract
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
  gen = AllocatorC . promote . gen
  alg = AllocatorC . (algA \/ (alg . handlePure runAllocatorC))
    where algA (Alloc name k) = promote (Located <$> runAllocatorC (alg (L (Alloc name gen))) <*> currentPackage <*> currentModule <*> pure name <*> ask >>= demote . runAllocatorC . k)


instance (Carrier (Deref value :+: sig) (DerefC (Evaluator term address value m)), Carrier sig m)
      => Carrier (Deref value :+: sig) (DerefC (Evaluator term (Located address) value m)) where
  gen = DerefC . promote . gen
  alg = DerefC . (algD \/ (alg . handlePure runDerefC))
    where algD (DerefCell cell k) = promote (runDerefC (alg (L (DerefCell cell gen))) >>= demote . runDerefC . k)
          algD (AssignCell value cell k) = promote (runDerefC (alg (L (AssignCell value cell gen))) >>= demote . runDerefC . k)
