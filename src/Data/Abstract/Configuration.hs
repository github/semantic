{-# LANGUAGE DeriveFoldable, DeriveGeneric, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module Data.Abstract.Configuration where

import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.Store
import Data.Functor.Classes.Generic
import GHC.Generics

data Configuration l t v
  = Configuration
    { configurationTerm :: t
    , configurationRoots :: [Address l v]
    , configurationEnvironment :: Environment l v
    , configurationStore :: Store l v
    }
    deriving (Generic1)

deriving instance (Eq l, Eq t, Eq v, Eq (Cell l v)) => Eq (Configuration l t v)
deriving instance (Ord l, Ord t, Ord v, Ord (Cell l v)) => Ord (Configuration l t v)
deriving instance (Show l, Show t, Show v, Show (Cell l v)) => Show (Configuration l t v)
deriving instance (Ord l, Foldable (Cell l)) => Foldable (Configuration l t)
instance (Eq l, Eq t, Eq1 (Cell l)) => Eq1 (Configuration l t) where liftEq = genericLiftEq
instance (Ord l, Ord t, Ord1 (Cell l)) => Ord1 (Configuration l t) where liftCompare = genericLiftCompare
instance (Show l, Show t, Show1 (Cell l)) => Show1 (Configuration l t) where liftShowsPrec = genericLiftShowsPrec
