{-# LANGUAGE MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.Env where

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Abstract.Environment
import Data.Abstract.Value

-- | 'Monad's offering a local environment binding variable names to addresses.
class Monad m => MonadEnv value m where
  -- | Retrieve the local environment.
  askEnv :: m (Environment (LocationFor value) value)

  -- | Run a computation with a locally-modified environment.
  localEnv :: (Environment (LocationFor value) value -> Environment (LocationFor value) value) -> m b -> m b

instance (Reader (Environment (LocationFor value) value) :< fs) => MonadEnv value (Eff fs) where
  askEnv = ask
  localEnv = local
