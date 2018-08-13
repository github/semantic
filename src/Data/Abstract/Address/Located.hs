{-# LANGUAGE GADTs, LambdaCase, RankNTypes, TypeOperators, UndecidableInstances #-}
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


instance (Allocatable address effects, Member (Reader ModuleInfo) effects, Member (Reader PackageInfo) effects, Member (Reader Span) effects) => Allocatable (Located address) effects where
  allocCell name = relocate (Located <$> allocCell name <*> currentPackage <*> currentModule <*> pure name <*> ask)

instance Derefable address effects => Derefable (Located address) effects where
  derefCell (Located loc _ _ _ _) = relocate . derefCell loc

  assignCell (Located loc _ _ _ _) value = relocate . assignCell loc value


relocate :: Evaluator address1 value effects a -> Evaluator address2 value effects a
relocate = raiseEff . lowerEff


runAllocator :: ( Member (Reader ModuleInfo) effects
                , Member (Reader PackageInfo) effects
                , Member (Reader Span) effects
                , PureEffects effects
                )
             => (forall x. Allocator address (Eff (Allocator address ': effects)) x -> Evaluator address value effects x)
             -> Evaluator (Located address) value (Allocator (Located address) ': effects) a
             -> Evaluator (Located address) value effects a
runAllocator handler = interpret (handleAllocator handler)

handleAllocator :: ( Member (Reader ModuleInfo) effects
                   , Member (Reader PackageInfo) effects
                   , Member (Reader Span) effects
                   )
                => (forall x. Allocator address (Eff (Allocator address ': effects)) x -> Evaluator address value effects x)
                -> Allocator (Located address) (Eff (Allocator (Located address) ': effects)) a
                -> Evaluator (Located address) value effects a
handleAllocator handler (Alloc name) = relocate (Located <$> handler (Alloc name) <*> currentPackage <*> currentModule <*> pure name <*> ask)

runDeref :: PureEffects effects
         => (forall x. Deref address value (Eff (Deref address value ': effects)) x -> Evaluator address value effects x)
         -> Evaluator (Located address) value (Deref (Located address) value ': effects) a
         -> Evaluator (Located address) value effects a
runDeref handler = interpret $ \case
  DerefCell  Located{..}       cell -> relocate (handler (DerefCell  address       cell))
  AssignCell Located{..} value cell -> relocate (handler (AssignCell address value cell))
