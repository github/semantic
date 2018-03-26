{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures #-}
module Analysis.Abstract.Quiet where

import Control.Abstract.Analysis
import Prologue

newtype Quiet m term value (effects :: [* -> *]) a = Quiet (m term value effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadControl term (m term value effects) => MonadControl term (Quiet m term value effects)
deriving instance MonadEnvironment value (m term value effects) => MonadEnvironment value (Quiet m term value effects)
deriving instance MonadHeap value (m term value effects) => MonadHeap value (Quiet m term value effects)
deriving instance MonadModuleTable term value (m term value effects) => MonadModuleTable term value (Quiet m term value effects)
deriving instance MonadEvaluator term value (m term value effects) => MonadEvaluator term value (Quiet m term value effects)
