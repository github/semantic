{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Analysis.Abstract.TypeChecking
( TypeChecking
) where

import Control.Abstract.Analysis
import Data.Abstract.Type
import Prologue hiding (TypeError)

newtype TypeChecking m (effects :: [* -> *]) a = TypeChecking (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh)

deriving instance MonadControl term (m effects)                    => MonadControl term (TypeChecking m effects)
deriving instance MonadEnvironment location value (m effects)      => MonadEnvironment location value (TypeChecking m effects)
deriving instance MonadHeap location value (m effects)             => MonadHeap location value (TypeChecking m effects)
deriving instance MonadModuleTable location term value (m effects) => MonadModuleTable location term value (TypeChecking m effects)
deriving instance MonadEvaluator location term value (m effects)   => MonadEvaluator location term value (TypeChecking m effects)

instance ( Effectful m
         , MonadAnalysis location term Type (m effects)
         , Member (Resumable (TypeError Type)) effects
         , MonadValue location Type (TypeChecking m effects)
         )
      => MonadAnalysis location term Type (TypeChecking m effects) where

  type Effects location term Type (TypeChecking m effects) = Resumable (TypeError Type) ': NonDet ': Effects location term Type (m effects)

  analyzeTerm eval term = resume @(TypeError Type) (liftAnalyze analyzeTerm eval term) (
      \yield err -> case err of
        NoValueError v -> yield "")
