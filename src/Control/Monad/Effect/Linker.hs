{-# LANGUAGE MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.Linker where

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Abstract.Linker


class Monad m => MonadLinker v m where
  askLinker :: m (Linker v)
  localLinker :: (Linker v -> Linker v) -> m b -> m b

instance (Reader (Linker v) :< fs) => MonadLinker v (Eff fs) where
  askLinker = ask
  localLinker = local
