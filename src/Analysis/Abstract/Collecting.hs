{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, TypeOperators #-}
module Analysis.Abstract.Collecting
( type Collecting
) where

import Control.Abstract.Analysis
import Prologue

newtype Collecting m term value (effects :: [* -> *]) a = Collecting (m term value effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadEvaluator term value (m term value effects) => MonadEvaluator term value (Collecting m term value effects)


instance ( MonadAnalysis term value (m term value effects)
         )
         => MonadAnalysis term value (Collecting m term value effects) where
  type RequiredEffects term value (Collecting m term value effects) = RequiredEffects term value (m term value effects)

  analyzeTerm term = liftAnalyze analyzeTerm term
