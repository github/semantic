{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Carrier.Store.Precise
( -- * Store carrier
  PAddr(..)
, PStore(..)
, PEnv
, runStore
, StoreC(StoreC)
  -- * Store effect
, module Analysis.Effect.Store
  -- * Env carrier
, runEnv
, EnvC(..)
  -- * Env effect
, module Analysis.Effect.Env
) where

import Analysis.Effect.Env
import Analysis.Effect.Store
import Analysis.Name
import Control.Algebra
import Control.Carrier.Reader
import Control.Carrier.State.Church
import Control.Effect.Labelled
import Control.Monad.Fail as Fail
import Data.IntMap as IntMap
import Data.Map as Map

newtype PAddr = PAddr Int

newtype PStore a = PStore (IntMap.IntMap (Maybe a))
  deriving (Eq, Ord, Monoid, Semigroup, Show)

type PEnv = Map.Map Name PAddr


-- Store carrier

runStore :: Applicative m => Labelled Store (StoreC val) m a -> m (PStore val, a)
runStore = runState (curry pure) mempty . runStoreC . runLabelled

newtype StoreC val m a = StoreC { runStoreC :: StateC (PStore val) m a }
  deriving (Applicative, Functor, Monad)

instance Algebra sig m => Algebra (Store PAddr val :+: sig) (StoreC val m) where
  alg hdl sig ctx = StoreC $ case sig of
    L op        -> case op of
      Alloc _ -> StateC $ \ k (PStore heap) -> do
        let a = maybe 0 ((+ 1) . fst) (IntMap.lookupMax heap)
        k (PStore (IntMap.insert a Nothing heap)) (PAddr a <$ ctx)
      Assign (PAddr a) v -> ctx <$ modify (\ (PStore heap) -> PStore (IntMap.insert a (Just v) heap))
      Fetch (PAddr a) -> gets (\ (PStore heap) -> maybe (error "unallocated addr") (maybe (error "uninitialized addr") (<$ ctx)) (IntMap.lookup a heap))
    R other -> alg (runStoreC . hdl) (R other) ctx


-- Env carrier

runEnv :: EnvC m a -> m a
runEnv = runReader Map.empty . runEnvC

newtype EnvC m a = EnvC { runEnvC :: ReaderC PEnv m a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail)

instance Algebra sig m => Algebra (Env PAddr :+: sig) (EnvC m) where
  alg hdl sig ctx = EnvC $ case sig of
    L (Bind name addr m) -> local (Map.insert name addr) (runEnvC (hdl (m <$ ctx)))
    L (Lookup name)      -> asks ((<$ ctx) . Map.lookup name)
    R other              -> alg (runEnvC . hdl) (R other) ctx
