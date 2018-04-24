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
         , Alternative (m effects)
         , MonadAnalysis location term value (m effects)
         , Member (Resumable TypeError) effects
         , Member NonDet effects
         , Member Fail effects
         , MonadValue location value (TypeChecking m effects)
         , value ~ Type
         )
      => MonadAnalysis location term value (TypeChecking m effects) where

  type Effects location term value (TypeChecking m effects) = Resumable TypeError ': Effects location term value (m effects)

  analyzeTerm eval term =
    resume @TypeError (liftAnalyze analyzeTerm eval term) (
        \yield err -> case err of
          NoValueError _ a -> yield a
          -- TODO: These should all yield both sides of the exception,
          -- but something is mysteriously busted in the innards of typechecking,
          -- so doing that just yields an empty list in the result type, which isn't
          -- extraordinarily helpful. Better for now to just die with an error and
          -- tackle this issue in a separate PR.
          BitOpError{}       -> throwResumable err
          NumOpError{}       -> throwResumable err
          UnificationError{} -> throwResumable err
        )

  analyzeModule = liftAnalyze analyzeModule
