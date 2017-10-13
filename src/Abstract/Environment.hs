{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, DeriveFoldable, DeriveFunctor, DeriveTraversable, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Abstract.Environment where

import Abstract.Store
import Abstract.FreeVariables
import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Functor.Classes
import Data.Functor.Classes.Show.Generic
import Data.Pointed
import Data.Semigroup
import GHC.Generics
import qualified Data.Map as Map


newtype Environment l a = Environment { unEnvironment :: Map.Map Name (Address l a) }
  deriving (Eq, Foldable, Functor, Monoid, Ord, Semigroup, Show, Traversable, Generic1)

envLookup :: Name -> Environment l a -> Maybe (Address l a)
envLookup = (. unEnvironment) . Map.lookup

envInsert :: Name -> Address l a -> Environment l a -> Environment l a
envInsert name value (Environment m) = Environment (Map.insert name value m)

envRoots :: (Foldable t, Ord l) => Environment l a -> t Name -> Set (Address l a)
envRoots env = foldr ((<>) . maybe mempty point . flip envLookup env) mempty


class Monad m => MonadEnv l a m where
  askEnv :: m (Environment l a)
  localEnv :: (Environment l a -> Environment l a) -> m b -> m b

instance (Reader (Environment l a) :< fs) => MonadEnv l a (Eff fs) where
  askEnv = ask
  localEnv = local


-- Instances

instance Eq2 Environment where
  liftEq2 eqL eqA (Environment m1) (Environment m2) = liftEq (liftEq2 eqL eqA) m1 m2

instance Eq l => Eq1 (Environment l) where
  liftEq = liftEq2 (==)

instance Ord2 Environment where
  liftCompare2 compareL compareA (Environment m1) (Environment m2) = liftCompare (liftCompare2 compareL compareA) m1 m2

instance Ord l => Ord1 (Environment l) where
  liftCompare = liftCompare2 compare

instance Show l => Show1 (Environment l) where
  liftShowsPrec = genericLiftShowsPrec
