{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Tracing
( type Tracing
) where

import Control.Abstract.Analysis
import Control.Monad.Effect.Writer
import Data.Abstract.Configuration
import Data.Abstract.Value
import Data.Semigroup.Reducer as Reducer
import Prologue

-- | Trace analysis.
--
--   Instantiating @trace@ to @[]@ yields a linear trace analysis, while @Set@ yields a reachable state analysis.
newtype Tracing (trace :: * -> *) m term value (effects :: [* -> *]) a = Tracing (m term value effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadEnvironment value (m term value effects) => MonadEnvironment value (Tracing trace m term value effects)
deriving instance MonadStore value (m term value effects) => MonadStore value (Tracing trace m term value effects)
deriving instance MonadEvaluator term value (m term value effects) => MonadEvaluator term value (Tracing trace m term value effects)

instance ( Corecursive term
         , Effectful (m term value)
         , Member (Writer (trace (ConfigurationFor term value))) effects
         , MonadAnalysis term value (m term value effects)
         , Ord (LocationFor value)
         , Reducer (ConfigurationFor term value) (trace (ConfigurationFor term value))
         )
         => MonadAnalysis term value (Tracing trace m term value effects) where
  type RequiredEffects term value (Tracing trace m term value effects) = Writer (trace (ConfigurationFor term value)) ': RequiredEffects term value (m term value effects)

  analyzeTerm term = do
    config <- getConfiguration (embedSubterm term)
    trace (Reducer.unit config)
    liftAnalyze analyzeTerm term

-- | Log the given trace of configurations.
trace :: ( Effectful (m term value)
         , Member (Writer (trace (ConfigurationFor term value))) effects
         )
      => trace (ConfigurationFor term value)
      -> Tracing trace m term value effects ()
trace = raise . tell
