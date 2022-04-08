{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Carrier.Store.Monovariant
( -- * Store carrier
  MAddr(..)
, MStore(..)
, runStoreState
, runStore
, StoreC(..)
  -- * Store effect
, module Analysis.Effect.Store
  -- * Env carrier
, EnvC(..)
  -- * Env effect
, module Analysis.Effect.Env
  -- * Running
, runFiles
) where

import Analysis.Effect.Env
import Analysis.Effect.Store
import Analysis.File (File)
import Analysis.Name
import Control.Algebra
import Control.Carrier.State.Church
import Control.Effect.Labelled
import Control.Effect.NonDet
import Control.Monad.Fail as Fail
import Data.Map as Map
import Data.Set as Set

newtype MAddr = MAddr { getMAddr :: Name }
  deriving (Eq, Ord, Show)

newtype MStore value = MStore { getMStore :: Map.Map MAddr (Set.Set value) }
  deriving (Eq, Ord, Show)

instance Ord value => Semigroup (MStore value) where
  MStore s1 <> MStore s2 = MStore (Map.unionWith Set.union s1 s2)

instance Ord value => Monoid (MStore value) where
  mempty = MStore Map.empty


-- Store carrier

runStoreState :: Applicative m => StateC (MStore value) m a -> m (MStore value, a)
runStoreState = runState (curry pure) (MStore Map.empty)

runStore :: Labelled Store (StoreC value) m a -> m a
runStore = runStoreC . runLabelled

newtype StoreC value m a = StoreC { runStoreC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail)

instance (Has (State (MStore value)) sig m, Alternative m, Ord value) => Algebra (Store MAddr value :+: sig) (StoreC value m) where
  alg hdl sig ctx = StoreC $ do
    MStore store <- get @(MStore value)
    case sig of
      L op        -> case op of
        Alloc name        -> let addr = MAddr name in addr <$ ctx <$ put (MStore (Map.insertWith Set.union addr Set.empty store))
        Assign addr value -> ctx <$ put (MStore (Map.insertWith Set.union addr (Set.singleton value) store))
        Fetch addr        -> foldMapA ((<$ put (MStore store)) . (<$ ctx)) (Map.findWithDefault Set.empty addr store)

      R other -> alg (runStoreC . hdl) other ctx


-- Env carrier

newtype EnvC value m a = EnvC { runEnv :: m a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail)

instance Has (State (MStore value)) sig m
      => Algebra (Env MAddr :+: sig) (EnvC value m) where
  alg hdl sig ctx = case sig of
    L op    -> case op of
      Bind _ _ m -> hdl (m <$ ctx)
      Lookup n   -> do
        MStore store <- get @(MStore value)
        pure (MAddr n <$ Map.lookup (MAddr n) store <$ ctx)

    R other -> EnvC (alg (runEnv . hdl) other ctx)


-- Running

runFiles
  :: (forall sig m . Has (State (MStore  value)) sig m => File term -> m (File result))
  -> [File term]
  -> (MStore value, [File result])
runFiles runFile
  = run
  . runStoreState
  . traverse runFile
