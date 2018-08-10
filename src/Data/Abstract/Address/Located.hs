{-# LANGUAGE UndecidableInstances #-}
module Data.Abstract.Address.Located
( Located(..)
) where

import Control.Abstract.Addressable
import Control.Abstract.Context
import Control.Abstract.Evaluator
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
