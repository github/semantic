{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.Fresh where

import Control.Effect
import Control.Monad.Effect.Internal

type TName = Int

data Fresh a where
  Reset :: TName -> Fresh ()
  Fresh :: Fresh TName

class Monad m => MonadFresh m where
  fresh :: m TName
  reset :: TName -> m ()

instance (Fresh :< fs) => MonadFresh (Eff fs) where
  fresh = send Fresh
  reset = send . Reset


instance RunEffect Fresh a where
  runEffect = relayState (0 :: TName) (const pure) (\ s action k -> case action of
    Fresh -> k (succ s) s
    Reset s' -> k s' ())
