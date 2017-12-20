{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.GC where

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Abstract.Live
import Data.Abstract.Value
import Data.Semigroup ((<>))

class Monad m => MonadGC a m where
  askRoots :: m (Live (LocationFor a) a)

  extraRoots :: Live (LocationFor a) a -> m b -> m b

instance (Ord (LocationFor a), Reader (Live (LocationFor a) a) :< fs) => MonadGC a (Eff fs) where
  askRoots = ask :: Eff fs (Live (LocationFor a) a)

  extraRoots roots' = local (<> roots')
