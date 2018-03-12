{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
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
  type RequiredEffects term value (Collecting m term value effects) = Reader (Live (LocationFor value) value) ': RequiredEffects term value (m term value effects)

  analyzeTerm term = liftAnalyze analyzeTerm term


-- | 'Monad's offering a local set of 'Live' (rooted/reachable) addresses.
class Monad m => MonadGC value m where
  -- | Retrieve the local 'Live' set.
  askRoots :: m (Live (LocationFor value) value)

  -- | Run a computation with the given 'Live' set added to the local root set.
  extraRoots :: Live (LocationFor value) value -> m a -> m a

instance (Effectful m, Monad (m effects), Ord (LocationFor value), Reader (Live (LocationFor value) value) :< effects) => MonadGC value (m effects) where
  askRoots = raise ask

  extraRoots roots = raise . local (<> roots) . lower
