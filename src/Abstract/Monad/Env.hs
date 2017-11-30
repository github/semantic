{-# LANGUAGE MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Abstract.Monad.Env where

import Abstract.Environment
import Control.Monad.Effect
import Control.Monad.Effect.Reader

class Monad m => MonadEnv l value m where
  askEnv :: m (Environment l value)
  localEnv :: (Environment l value -> Environment l value) -> m b -> m b

instance (Reader (Environment l value) :< fs) => MonadEnv l value (Eff fs) where
  askEnv = ask
  localEnv = local
