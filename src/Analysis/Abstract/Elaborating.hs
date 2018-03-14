{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, UndecidableInstances #-}
module Analysis.Abstract.Elaborating
( type Elaborating
) where

import Control.Abstract.Analysis
import Control.Abstract.Value
import Data.Term
import Prologue

newtype Elaborating m term value (effects :: [* -> *]) a = Elaborating (m term value effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadControl term (m term value effects) => MonadControl term (Elaborating m term value effects)
deriving instance MonadEnvironment value (m term value effects) => MonadEnvironment value (Elaborating m term value effects)
deriving instance MonadStore value (m term value effects) => MonadStore value (Elaborating m term value effects)
deriving instance MonadModuleTable term value (m term value effects) => MonadModuleTable term value (Elaborating m term value effects)
deriving instance MonadEvaluator term value (m term value effects) => MonadEvaluator term value (Elaborating m term value effects)

instance MonadAnalysis term value (m term value effects)
         => MonadAnalysis term value (Elaborating m term value effects) where
  type RequiredEffects term value (Elaborating m term value effects) = RequiredEffects term value (m term value effects)
  analyzeTerm = liftAnalyze analyzeTerm

instance ( elab ~ Term (Base term) value
         , MonadAnalysis term elab (m term elab effects)
         , Recursive term
         , Show1 (Base term)
         , Show value
         )
         => MonadValue term elab (Elaborating m term elab effects) where
