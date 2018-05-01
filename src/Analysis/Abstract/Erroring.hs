{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
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

instance Interpreter               m                    effects
      => Interpreter (Erroring exc m) (Resumable exc ': effects) where
  type Result (Erroring exc m) (Resumable exc ': effects) result = Result m effects (Either (SomeExc exc) result)
  interpret = interpret . runErroring . raiseHandler runError
