{-# LANGUAGE MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Abstract.Monad.Env where

import Abstract.Environment
import Abstract.Value
import Control.Monad.Effect
import Control.Monad.Effect.Reader

class Monad m => MonadEnv value m where
  askEnv :: m (Environment (LocationFor value) value)
  localEnv :: (Environment (LocationFor value) value -> Environment (LocationFor value) value) -> m b -> m b

instance (Reader (Environment (LocationFor value) value) :< fs) => MonadEnv value (Eff fs) where
  askEnv = ask
  localEnv = local
