{-# LANGUAGE TypeOperators, GADTs, RankNTypes #-}
module Semantic.Timeout
( timeout
, Timeout
, runTimeout
) where

import Prologue hiding (MonadError (..))

import           Control.Monad.Effect
import           Control.Monad.IO.Class
import qualified System.Timeout as System


data Timeout task output where
   Timeout :: Int -> task output -> Timeout task (Maybe output)

timeout :: (Member Timeout effs) => Int -> Eff effs output -> Eff effs (Maybe output)
timeout time = send . Timeout time

instance PureEffect Timeout
instance Effect Timeout where
  handleState c dist (Request (Timeout i task) k) = Request (Timeout i (dist (task <$ c))) (dist . maybe (k Nothing <$ c) (fmap (k . Just)))

runTimeout :: (Member (Lift IO) effects, PureEffects effects)
  => (forall x . Eff effects x -> IO x)
  -> Eff (Timeout ': effects) a
  -> Eff effects a
runTimeout handler = interpret (\ (Timeout i task) -> liftIO (System.timeout i (handler (runTimeout handler task))))
