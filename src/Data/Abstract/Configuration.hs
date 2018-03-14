{-# LANGUAGE DeriveFoldable, DeriveGeneric, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module Data.Abstract.Configuration where

import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.Live
import Data.Abstract.Store
import Data.Abstract.Value
import Prologue

-- | The configuration for term and abstract value types.
type ConfigurationFor term value = Configuration (LocationFor value) term value

-- | A single point in a program’s execution.
data Configuration location term value
  = Configuration
    { configurationTerm        :: term                                 -- ^ The “instruction,” i.e. the current term to evaluate.
    , configurationRoots       :: Live location value                  -- ^ The set of rooted addresses.
    , configurationEnvironment :: Environment (Address location value) -- ^ The environment binding any free variables in 'configurationTerm'.
    , configurationStore       :: Store location value                 -- ^ The store of values.
    }
    deriving (Generic1)

deriving instance (Eq location, Eq term, Eq value, Eq (Cell location value)) => Eq (Configuration location term value)
deriving instance (Ord location, Ord term, Ord value, Ord (Cell location value)) => Ord (Configuration location term value)
deriving instance (Show location, Show term, Show value, Show (Cell location value)) => Show (Configuration location term value)
deriving instance (Ord location, Foldable (Cell location)) => Foldable (Configuration location term)
instance (Eq location, Eq term, Eq1 (Cell location)) => Eq1 (Configuration location term) where liftEq = genericLiftEq
instance (Ord location, Ord term, Ord1 (Cell location)) => Ord1 (Configuration location term) where liftCompare = genericLiftCompare
instance (Show location, Show term, Show1 (Cell location)) => Show1 (Configuration location term) where liftShowsPrec = genericLiftShowsPrec
