{-# LANGUAGE MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.Trace where

import Control.Monad.Effect
import Control.Monad.Effect.Writer
import Data.Abstract.Configuration
import Data.Abstract.Value

-- | 'Monad's offering a writable trace of configurations.
--
--   @t@ is the type of terms, @v@ the type of values, @g@ the type of the collection represented by the log (e.g. '[]' for regular traces, or @Set@ for the trace of reachable states).
class Monad m => MonadTrace t v g m where
  -- | Log the given collection of configurations.
  trace :: g (Configuration (LocationFor v) t v) -> m ()

instance (Writer (g (Configuration (LocationFor v) t v)) :< fs) => MonadTrace t v g (Eff fs) where
  trace = tell
