{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.GC where

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Abstract.Address
import Data.Abstract.Value
import Data.Semigroup ((<>))
import Data.Set (Set)

class Monad m => MonadGC a m where
  askRoots :: m (Set (Address (LocationFor a) a))

  extraRoots :: Set (Address (LocationFor a) a) -> m b -> m b

instance (Ord (LocationFor a), Reader (Set (Address (LocationFor a) a)) :< fs) => MonadGC a (Eff fs) where
  askRoots = ask :: Eff fs (Set (Address (LocationFor a) a))

  extraRoots roots' = local (<> roots')
