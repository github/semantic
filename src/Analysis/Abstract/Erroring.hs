{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Erroring
( Erroring
) where

import Control.Abstract.Analysis
import Prologue

-- | An analysis that fails on errors.
newtype Erroring (exc :: * -> *) m (effects :: [* -> *]) a = Erroring { runErroring :: m effects a }
  deriving (Alternative, Applicative, Effectful, Functor, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (Erroring exc m)
deriving instance MonadAnalysis location term value effects m => MonadAnalysis location term value effects (Erroring exc m)

instance Interpreter                   effects  (Either (SomeExc exc) result) rest               m
      => Interpreter (Resumable exc ': effects)                       result  rest (Erroring exc m) where
  interpret = interpret . runErroring . raiseHandler runError
