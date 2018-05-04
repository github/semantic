{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Erroring
( Erroring
) where

import Control.Abstract.Analysis
import Prologue

-- | An analysis that fails on errors.
newtype Erroring (exc :: * -> *) m (effects :: [* -> *]) a = Erroring { runErroring :: m effects a }
  deriving (Alternative, Applicative, Effectful, Functor, Monad)

deriving instance Evaluator location term value m => Evaluator location term value (Erroring exc m)
deriving instance AnalyzeModule location term value inner outer m => AnalyzeModule location term value inner outer (Erroring exc m)
deriving instance AnalyzeTerm location term value inner outer m => AnalyzeTerm location term value inner outer (Erroring exc m)

instance Interpreter               m                    effects
      => Interpreter (Erroring exc m) (Resumable exc ': effects) where
  type Result (Erroring exc m) (Resumable exc ': effects) result = Result m effects (Either (SomeExc exc) result)
  interpret = interpret . runErroring . raiseHandler runError
