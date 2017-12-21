{-# LANGUAGE MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.Trace where

import Control.Monad.Effect
import Control.Monad.Effect.Writer
import Data.Abstract.Configuration
import Data.Abstract.Value

-- | 'Monad's offering a writable trace of configurations.
class Monad m => MonadTrace t v g m where
  trace :: g (Configuration (LocationFor v) t v) -> m ()

instance (Writer (g (Configuration (LocationFor v) t v)) :< fs) => MonadTrace t v g (Eff fs) where
  trace = tell
