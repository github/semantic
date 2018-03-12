{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, TypeOperators #-}
module Analysis.Abstract.Collecting
( type Collecting
) where

import Control.Abstract.Analysis
import Data.Abstract.Live
import Data.Abstract.Value
import Prologue

newtype Collecting m term value (effects :: [* -> *]) a = Collecting (m term value effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadEvaluator term value (m term value effects) => MonadEvaluator term value (Collecting m term value effects)


instance ( MonadAnalysis term value (m term value effects)
         )
         => MonadAnalysis term value (Collecting m term value effects) where
  type RequiredEffects term value (Collecting m term value effects) = RequiredEffects term value (m term value effects)

  analyzeTerm term = liftAnalyze analyzeTerm term


-- | 'Monad's offering a local set of 'Live' (rooted/reachable) addresses.
class Monad m => MonadGC value m where
  -- | Retrieve the local 'Live' set.
  askRoots :: m (Live (LocationFor value) value)

  -- | Run a computation with the given 'Live' set added to the local root set.
  extraRoots :: Live (LocationFor value) value -> m a -> m a
