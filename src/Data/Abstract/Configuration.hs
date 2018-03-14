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
data Configuration l t v
  = Configuration
    { configurationTerm :: t                      -- ^ The “instruction,” i.e. the current term to evaluate.
    , configurationRoots :: Live l v              -- ^ The set of rooted addresses.
    , configurationEnvironment :: Environment l v -- ^ The environment binding any free variables in 'configurationTerm'.
    , configurationStore :: Store l v             -- ^ The store of values.
    }
    deriving (Generic1)

deriving instance (Eq l, Eq t, Eq v, Eq (Cell l v)) => Eq (Configuration l t v)
deriving instance (Ord l, Ord t, Ord v, Ord (Cell l v)) => Ord (Configuration l t v)
deriving instance (Show l, Show t, Show v, Show (Cell l v)) => Show (Configuration l t v)
deriving instance (Ord l, Foldable (Cell l)) => Foldable (Configuration l t)
instance (Eq l, Eq t, Eq1 (Cell l)) => Eq1 (Configuration l t) where liftEq = genericLiftEq
instance (Ord l, Ord t, Ord1 (Cell l)) => Ord1 (Configuration l t) where liftCompare = genericLiftCompare
instance (Show l, Show t, Show1 (Cell l)) => Show1 (Configuration l t) where liftShowsPrec = genericLiftShowsPrec
