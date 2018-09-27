{-# LANGUAGE GADTs, RankNTypes, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Address.Located
( Located(..)
, runAllocator
, handleAllocator
, runDeref
, handleDeref
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


relocate :: Evaluator term address1 value effects a -> Evaluator term address2 value effects a
relocate = raiseEff . lowerEff


runAllocator :: ( Member (Reader ModuleInfo) effects
                , Member (Reader PackageInfo) effects
                , Member (Reader Span) effects
                , PureEffects effects
                )
             => (forall x. Allocator address (Eff (Allocator address ': effects)) x -> Evaluator term address value effects x)
             -> Evaluator term (Located address) value (Allocator (Located address) ': effects) a
             -> Evaluator term (Located address) value effects a
runAllocator handler = interpret (handleAllocator handler)

handleAllocator :: ( Member (Reader ModuleInfo) effects
                   , Member (Reader PackageInfo) effects
                   , Member (Reader Span) effects
                   )
                => (forall x. Allocator address (Eff (Allocator address ': effects)) x -> Evaluator term address value effects x)
                -> Allocator (Located address) (Eff (Allocator (Located address) ': effects)) a
                -> Evaluator term (Located address) value effects a
handleAllocator handler (Alloc name) = relocate (Located <$> handler (Alloc name) <*> currentPackage <*> currentModule <*> pure name <*> ask)

runDeref :: PureEffects effects
         => (forall x. Deref value (Eff (Deref value ': effects)) x -> Evaluator term address value effects x)
         -> Evaluator term (Located address) value (Deref value ': effects) a
         -> Evaluator term (Located address) value effects a
runDeref handler = interpret (handleDeref handler)

handleDeref :: (forall x. Deref value (Eff (Deref value ': effects)) x -> Evaluator term address value effects x)
            -> Deref value (Eff (Deref value ': effects)) a
            -> Evaluator term (Located address) value effects a
handleDeref handler (DerefCell        cell) = relocate (handler (DerefCell        cell))
handleDeref handler (AssignCell value cell) = relocate (handler (AssignCell value cell))
