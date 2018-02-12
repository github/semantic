{-# LANGUAGE MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.Linker where

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Abstract.Linker
import Data.Abstract.Value

class Monad m => MonadLinker value m where
  askLinker :: m (Linker value)
  localLinker :: ((Linker value) -> (Linker value)) -> m b -> m b

instance (Reader (Linker value) :< fs) => MonadLinker value (Eff fs) where
  askLinker = ask
  localLinker = local
