{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Addressable
( Allocatable(..)
, Derefable(..)
) where

import Control.Abstract.Context
import Control.Abstract.Evaluator
import Control.Abstract.Hole
import Data.Abstract.Address
import Data.Abstract.Name
import qualified Data.Set as Set
import Prologue

class (Ord address, Show address) => Allocatable address effects where
  allocCell :: Name -> Evaluator address value effects address

  assignCell :: Ord value => address -> value -> Set value -> Evaluator address value effects (Set value)

class (Ord address, Show address) => Derefable address effects where
  derefCell :: address -> Set value -> Evaluator address value effects (Maybe value)


instance Member Fresh effects => Allocatable Precise effects where
  allocCell _ = Precise <$> fresh

  assignCell _ value _ = pure (Set.singleton value)

instance Derefable Precise effects where
  derefCell _ = pure . fmap fst . Set.minView


instance Allocatable Monovariant effects where
  allocCell = pure . Monovariant

  assignCell _ value values = pure (Set.insert value values)

instance Member NonDet effects => Derefable Monovariant effects where
  derefCell _ = traverse (foldMapA pure) . nonEmpty . toList


instance (Allocatable address effects, Member (Reader ModuleInfo) effects, Member (Reader PackageInfo) effects, Member (Reader Span) effects) => Allocatable (Located address) effects where
  allocCell name = relocate (Located <$> allocCell name <*> currentPackage <*> currentModule <*> pure name <*> ask)

  assignCell (Located loc _ _ _ _) value = relocate . assignCell loc value

instance Derefable address effects => Derefable (Located address) effects where
  derefCell (Located loc _ _ _ _) = relocate . derefCell loc


instance (Allocatable address effects, Ord context, Show context) => Allocatable (Hole context address) effects where
  allocCell name = relocate (Total <$> allocCell name)

  assignCell (Total loc) value = relocate . assignCell loc value
  assignCell (Partial _) _ = pure

instance (Derefable address effects, Ord context, Show context) => Derefable (Hole context address) effects where
  derefCell (Total loc) = relocate . derefCell loc
  derefCell (Partial _) = const (pure Nothing)

relocate :: Evaluator address1 value effects a -> Evaluator address2 value effects a
relocate = raiseEff . lowerEff
