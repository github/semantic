module Control.Abstract.Addressable
( Allocatable(..)
, Derefable(..)
) where

import Control.Abstract.Evaluator
import Data.Abstract.Name
import Prologue

class (Ord address, Show address) => Allocatable address effects where
  allocCell :: Name -> Evaluator address value effects address

  assignCell :: Ord value => address -> value -> Set value -> Evaluator address value effects (Set value)

class (Ord address, Show address) => Derefable address effects where
  derefCell :: address -> Set value -> Evaluator address value effects (Maybe value)
