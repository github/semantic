{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies #-}
module Analysis.Abstract.Elaborating
( type Elaborating
) where

import Control.Abstract.Analysis
import Prologue

newtype Elaborating m term value (effects :: [* -> *]) a = Elaborating (m term value effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadEnvironment value (m term value effects) => MonadEnvironment value (Elaborating m term value effects)
deriving instance MonadStore value (m term value effects) => MonadStore value (Elaborating m term value effects)
deriving instance MonadModuleTable term value (m term value effects) => MonadModuleTable term value (Elaborating m term value effects)
deriving instance MonadEvaluator term value (m term value effects) => MonadEvaluator term value (Elaborating m term value effects)
