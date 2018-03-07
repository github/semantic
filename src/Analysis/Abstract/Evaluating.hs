{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, MultiParamTypeClasses, UndecidableInstances #-}
module Analysis.Abstract.Evaluating where

import Control.Abstract.Evaluator
import Data.Abstract.Evaluatable
import Data.Abstract.Linker
import Data.Abstract.Value
import Data.Blob
import Prelude hiding (fail)
import qualified Data.Map as Map
import Prologue
import System.FilePath.Posix

-- | Evaluate a term to a value.
evaluate :: forall value term
         .  ( Evaluatable (Base term)
            , FreeVariables term
            , MonadAddressable (LocationFor value) value (Evaluation term value)
            , MonadValue term value (Evaluation term value)
            , Ord (LocationFor value)
            , Recursive term
            , Semigroup (CellFor value)
            )
         => term
         -> Final (EvaluatorEffects term value) value
evaluate = run @(EvaluatorEffects term value) . runEvaluator . runEvaluation . evaluateTerm

-- | Evaluate terms and an entry point to a value.
evaluates :: forall value term
          .  ( Evaluatable (Base term)
             , FreeVariables term
             , MonadAddressable (LocationFor value) value (Evaluation term value)
             , MonadValue term value (Evaluation term value)
             , Ord (LocationFor value)
             , Recursive term
             , Semigroup (CellFor value)
             )
          => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
          -> (Blob, term)   -- Entrypoint
          -> Final (EvaluatorEffects term value) value
evaluates pairs (_, t) = run @(EvaluatorEffects term value) (runEvaluator (runEvaluation (withModules pairs (evaluateTerm t))))

-- | Run an action with the passed ('Blob', @term@) pairs available for imports.
withModules :: (MonadAnalysis term value m, MonadEvaluator term value m) => [(Blob, term)] -> m a -> m a
withModules pairs = localModuleTable (const moduleTable)
  where moduleTable = Linker (Map.fromList (map (first (dropExtensions . blobPath)) pairs))

-- | An analysis performing concrete evaluation of @term@s to @value@s.
newtype Evaluation term value a = Evaluation { runEvaluation :: Evaluator term value (EvaluatorEffects term value) a }
  deriving (Applicative, Functor, Monad, MonadFail)

deriving instance MonadEvaluator term value (Evaluation term value)

instance ( Evaluatable (Base term)
         , FreeVariables term
         , MonadAddressable (LocationFor value) value (Evaluation term value)
         , MonadValue term value (Evaluation term value)
         , Recursive term
         , Semigroup (CellFor value)
         )
         => MonadAnalysis term value (Evaluation term value) where
  analyzeTerm = eval
