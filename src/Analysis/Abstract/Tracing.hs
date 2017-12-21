{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies #-}
module Analysis.Abstract.Tracing where

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Address
import Control.Monad.Effect.Env
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.Store
import Control.Monad.Effect.State
import Control.Monad.Effect.Trace
import Control.Monad.Effect.Writer
import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.Environment
import Data.Abstract.Eval
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Function (fix)
import Data.Functor.Foldable (Base, Recursive(..))
import Data.Pointed
import Data.Semigroup
import Data.Set

-- | The effects necessary for tracing analyses.
type TracingInterpreter t v g
  = '[ Writer (g (Configuration (LocationFor v) t v)) -- For 'MonadTrace'.
     , Fail                                           -- For 'MonadFail'.
     , State (Store (LocationFor v) v)                -- For 'MonadStore'.
     , Reader (Environment (LocationFor v) v)         -- For 'MonadEnv'.
     ]

-- | The effects necessary for a linear trace analysis.
type TraceInterpreter t v = TracingInterpreter t v []

-- | The effects necessary for a reachable state analysis.
type ReachableStateInterpreter t v = TracingInterpreter t v Set


-- | Tracing state analyses
evalTrace :: forall v term
          . ( Ord v, Ord term, Ord (Cell (LocationFor v) v)
            , Functor (Base term)
            , Recursive term
            , MonadAddress (LocationFor v) (Eff (TraceInterpreter term v))
            , MonadGC v (Eff (TraceInterpreter term v))
            , Semigroup (Cell (LocationFor v) v)
            , Eval term v (Eff (TraceInterpreter term v)) (Base term)
            )
          => term -> Final (TracingInterpreter term v []) v
evalTrace = run @(TraceInterpreter term v) . fix (evTell @[] (\ recur yield -> eval recur yield . project)) pure

-- | Reach state analyses
evalReach :: forall v term
          . ( Ord v, Ord term, Ord (LocationFor v), Ord (Cell (LocationFor v) v)
            , Functor (Base term)
            , Recursive term
            , MonadAddress (LocationFor v) (Eff (ReachableStateInterpreter term v))
            , MonadGC v (Eff (ReachableStateInterpreter term v))
            , Semigroup (Cell (LocationFor v) v)
            , Eval term v (Eff (ReachableStateInterpreter term v)) (Base term)
            )
          => term -> Final (TracingInterpreter term v Set) v
evalReach = run @(ReachableStateInterpreter term v) . fix (evTell @Set (\ recur yield -> eval recur yield . project)) pure


-- | Small-step evaluation which records every visited configuration.
evTell :: forall g t m v
       . ( Monoid (g (Configuration (LocationFor v) t v))
         , Pointed g
         , MonadTrace t v g m
         , MonadEnv v m
         , MonadStore v m
         , MonadGC v m
         )
       => (((v -> m v) -> t -> m v) -> (v -> m v) -> t -> m v)
       -> ((v -> m v) -> t -> m v)
       -> (v -> m v) -> t -> m v
evTell ev0 ev' yield e = do
  env <- askEnv
  store <- getStore
  roots <- askRoots
  trace (point (Configuration e roots env store) :: g (Configuration (LocationFor v) t v))
  ev0 ev' yield e
