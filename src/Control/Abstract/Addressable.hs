{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Addressable
( Addressable(..)
, Allocatable(..)
, Derefable(..)
) where

import Control.Abstract.Context
import Control.Abstract.Evaluator
import Control.Abstract.Hole
import Data.Abstract.Address
import Data.Abstract.Name
import Prologue

-- | Defines allocation and dereferencing of addresses.
class (Ord address, Show address) => Addressable address (effects :: [(* -> *) -> * -> *]) where
  -- | The type into which stored values will be written for a given address type.
  type family Cell address :: * -> *

class Addressable address effects => Allocatable address effects where
  allocCell :: Name -> Evaluator address value effects address

class Addressable address effects => Derefable address effects where
  derefCell :: address -> Cell address value -> Evaluator address value effects (Maybe value)


-- | 'Precise' addresses are always allocated a fresh address, and dereference to the 'Latest' value written.
instance Addressable Precise effects where
  type Cell Precise = Latest

instance Member Fresh effects => Allocatable Precise effects where
  allocCell _ = Precise <$> fresh

instance Derefable Precise effects where
  derefCell _ = pure . getLast . unLatest

-- | 'Monovariant' addresses allocate one address per unique variable name, and dereference once per stored value, nondeterministically.
instance Addressable Monovariant effects where
  type Cell Monovariant = All

instance Allocatable Monovariant effects where
  allocCell = pure . Monovariant

instance Member NonDet effects => Derefable Monovariant effects where
  derefCell _ = traverse (foldMapA pure) . nonEmpty . toList

-- | 'Located' addresses allocate & dereference using the underlying address, contextualizing addresses with the current 'PackageInfo' & 'ModuleInfo'.
instance Addressable address effects => Addressable (Located address) effects where
  type Cell (Located address) = Cell address

instance (Allocatable address effects, Member (Reader ModuleInfo) effects, Member (Reader PackageInfo) effects) => Allocatable (Located address) effects where
  allocCell name = relocate (Located <$> allocCell name <*> currentPackage <*> currentModule)

instance Derefable address effects => Derefable (Located address) effects where
  derefCell (Located loc _ _) = relocate . derefCell loc

instance (Addressable address effects, Ord context, Show context) => Addressable (Hole context address) effects where
  type Cell (Hole context address) = Cell address

instance (Allocatable address effects, Ord context, Show context) => Allocatable (Hole context address) effects where
  allocCell name = relocate (Total <$> allocCell name)

instance (Derefable address effects, Ord context, Show context) => Derefable (Hole context address) effects where
  derefCell (Total loc) = relocate . derefCell loc
  derefCell (Partial _) = const (pure Nothing)

relocate :: Evaluator address1 value effects a -> Evaluator address2 value effects a
relocate = raiseEff . lowerEff
