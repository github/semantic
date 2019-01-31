{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, LambdaCase,
             MultiParamTypeClasses, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}

-- | An effect that enables catching exceptions thrown from
-- impure computations such as IO.
module Control.Effect.Catch
  ( Catch (..)
  , catch
  , runCatch
  , CatchC (..)
  ) where

import           Control.Effect.Carrier
import           Control.Effect.Internal
import           Control.Effect.Sum
import qualified Control.Exception as Exc
import           Control.Monad.IO.Class

data Catch m k
  = forall output e . Exc.Exception e => CatchIO (m output) (e -> m output) (output -> k)

deriving instance Functor (Catch m)

instance HFunctor Catch where
  hmap f (CatchIO go cleanup k) = CatchIO (f go) (f . cleanup) k

instance Effect Catch where
  handle state handler (CatchIO go cleanup k)
    = CatchIO (handler (go <$ state)) (\se -> handler (cleanup se <$ state)) (handler . fmap k)

-- | Like 'Control.Effect.Error.catchError', but delegating to
-- 'Control.Exception.catch' under the hood, which allows catching
-- errors that might occur when lifting 'IO' computations.
-- Unhandled errors are rethrown. Use 'SomeException' if you want
-- to catch all errors.
catch :: (Member Catch sig, Carrier sig m, Exc.Exception e)
        => m a
        -> (e -> m a)
        -> m a
catch go cleanup = send (CatchIO go cleanup ret)

runCatch :: (Carrier sig m, MonadIO m)
         => (forall x . m x -> IO x)
         -> Eff (CatchC m) a
         -> m a
runCatch handler = runCatchC handler . interpret

newtype CatchC m a = CatchC ((forall x . m x -> IO x) -> m a)

runCatchC :: (forall x . m x -> IO x) -> CatchC m a -> m a
runCatchC handler (CatchC m) = m handler

instance (Carrier sig m, MonadIO m) => Carrier (Catch :+: sig) (CatchC m) where
  ret a = CatchC (const (ret a))
  eff op = CatchC (\ handler -> handleSum
    (eff . handlePure (runCatchC handler))
    (\case
        CatchIO go cleanup k -> liftIO (Exc.catch
                                         (handler (runCatchC handler go))
                                         (handler . runCatchC handler . cleanup))
                                >>= runCatchC handler . k
    ) op)
