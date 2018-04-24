{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables, TypeFamilies #-}
module Analysis.Abstract.Evaluating
( Evaluating
, EvaluatorState(..)
, State
) where

import Control.Abstract.Analysis
import Control.Monad.Effect
import Data.Abstract.Configuration
import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable
import Data.Abstract.Module
import Data.Abstract.ModuleTable
import Data.Abstract.Origin
import Prologue

-- | An analysis evaluating @term@s to @value@s with a list of @effects@ using 'Evaluatable', and producing incremental results of type @a@.
newtype Evaluating location term value effects a = Evaluating (Eff effects a)
  deriving (Applicative, Functor, Effectful, Monad)

deriving instance Member Fail   effects => MonadFail   (Evaluating location term value effects)
deriving instance Member NonDet effects => Alternative (Evaluating location term value effects)

-- | Effects necessary for evaluating (whether concrete or abstract).
type EvaluatingEffects location term value
  = '[ Exc (ControlThrow value)
     , Resumable (EvalError value)
     , Resumable (ResolutionError value)
     , Resumable (LoadError term value)
     , Resumable (ValueError location value)
     , Resumable (Unspecialized value)
     , Resumable (AddressError location value)
     , Fail                                        -- Failure with an error message
     , Fresh                                       -- For allocating new addresses and/or type variables.
     , Reader (SomeOrigin term)                    -- The current term’s origin.
     , Reader (ModuleTable [Module term])          -- Cache of unevaluated modules
     , Reader (Environment location value)         -- Default environment used as a fallback in lookupEnv
     , State  (EvaluatorState location term value) -- Environment, heap, modules, exports, and jumps.
     ]

instance ( Member Fail effects
         , Member (Reader (Environment location value)) effects
         , Member (Reader (ModuleTable [Module term])) effects
         , Member (Reader (SomeOrigin term)) effects
         , Member (State (EvaluatorState location term value)) effects
         )
      => MonadEvaluator location term value effects (Evaluating location term value) where
  getConfiguration term = Configuration term mempty <$> getEnv <*> getHeap

instance ( Corecursive term
         , Member Fail effects
         , Member (Reader (Environment location value)) effects
         , Member (Reader (ModuleTable [Module term])) effects
         , Member (Reader (SomeOrigin term)) effects
         , Member (State (EvaluatorState location term value)) effects
         , Recursive term
         )
      => MonadAnalysis location term value effects (Evaluating location term value) where
  type Effects location term value (Evaluating location term value) = EvaluatingEffects location term value

  analyzeTerm eval term = pushOrigin (termOrigin (embedSubterm term)) (eval term)

  analyzeModule eval m = pushOrigin (moduleOrigin (subterm <$> m)) (eval m)
