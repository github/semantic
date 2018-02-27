{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies #-}
module Analysis.Abstract.Tracing where

import Prologue
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

-- | The effects necessary for tracing analyses.
type Tracing g t v
  = '[ Writer (g (Configuration (LocationFor v) t v)) -- For 'MonadTrace'.
     , Fail                                           -- For 'MonadFail'.
     , State (Store (LocationFor v) v)                -- For 'MonadStore'.
     , Reader (Environment (LocationFor v) v)         -- For 'MonadEnv'.
     ]

-- | Linear trace analysis.
evalTrace :: forall v term
          . ( Ord v, Ord term, Ord (Cell (LocationFor v) v)
            , Functor (Base term)
            , Recursive term
            , MonadAddress (LocationFor v) (Eff (Tracing [] term v))
            , MonadGC v (Eff (Tracing [] term v))
            , Semigroup (Cell (LocationFor v) v)
            , Eval term v (Eff (Tracing [] term v)) (Base term)
            )
          => term -> Final (Tracing [] term v) v
evalTrace = run @(Tracing [] term v) . fix (evTell @[] (\ recur yield -> eval recur yield . project)) pure

-- | Reachable configuration analysis.
evalReach :: forall v term
          . ( Ord v, Ord term, Ord (LocationFor v), Ord (Cell (LocationFor v) v)
            , Functor (Base term)
            , Recursive term
            , MonadAddress (LocationFor v) (Eff (Tracing Set term v))
            , MonadGC v (Eff (Tracing Set term v))
            , Semigroup (Cell (LocationFor v) v)
            , Eval term v (Eff (Tracing Set term v)) (Base term)
            )
          => term -> Final (Tracing Set term v) v
evalReach = run @(Tracing Set term v) . fix (evTell @Set (\ recur yield -> eval recur yield . project)) pure


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
