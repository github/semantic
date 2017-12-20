{-# LANGUAGE MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.Env where

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Abstract.Environment
import Data.Abstract.Value

class Monad m => MonadEnv value m where
  askEnv :: m (Environment (LocationFor value) value)
  localEnv :: (Environment (LocationFor value) value -> Environment (LocationFor value) value) -> m b -> m b

instance (Reader (Environment (LocationFor value) value) :< fs) => MonadEnv value (Eff fs) where
  askEnv = ask
  localEnv = local
