{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s MonadEvaluator constraint
module Analysis.Abstract.BadLoads where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Prologue

newtype BadLoads m (effects :: [* -> *]) a = BadLoads { runBadLoads :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (BadLoads m)
deriving instance MonadAnalysis location term value effects m => MonadAnalysis location term value effects (BadLoads m)

instance ( Interpreter m effects
         , MonadEvaluator location term value effects m
         )
      => Interpreter (BadLoads m) (Resumable (LoadError term) ': effects) where
  type Result (BadLoads m) (Resumable (LoadError term) ': effects) result = Result m effects result
  interpret
    = interpret
    . runBadLoads
    . raiseHandler (relay pure (\ (Resumable err) yield -> traceM ("LoadError:" <> show err) *> case err of
      LoadError _ -> yield []))
