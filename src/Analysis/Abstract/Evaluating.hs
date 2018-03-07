{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, MultiParamTypeClasses, UndecidableInstances #-}
module Analysis.Abstract.Evaluating where

import Prologue
import Control.Abstract.Evaluator
import Data.Abstract.Address
import Data.Abstract.Evaluatable
import Data.Abstract.Linker
import Data.Abstract.Value
import Data.Blob
import Prelude hiding (fail)
import qualified Data.Map as Map
import System.FilePath.Posix

-- | Evaluate a term to a value.
evaluate :: forall v term
         .  ( Evaluatable (Base term)
            , FreeVariables term
            , MonadAddressable (LocationFor v) v (Evaluation term v)
            , MonadValue term v (Evaluation term v)
            , Ord (LocationFor v)
            , Recursive term
            , Semigroup (Cell (LocationFor v) v)
            )
         => term
         -> Final (EvaluatorEffects term v) v
evaluate = run @(EvaluatorEffects term v) . runEvaluator . runEvaluation . evaluateTerm

-- | Evaluate terms and an entry point to a value.
evaluates :: forall v term
          .  ( Evaluatable (Base term)
             , FreeVariables term
             , MonadAddressable (LocationFor v) v (Evaluation term v)
             , MonadValue term v (Evaluation term v)
             , Ord (LocationFor v)
             , Recursive term
             , Semigroup (Cell (LocationFor v) v)
             )
          => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
          -> (Blob, term)   -- Entrypoint
          -> Final (EvaluatorEffects term v) v
evaluates pairs (_, t) = run @(EvaluatorEffects term v) (runEvaluator (runEvaluation (withModules pairs (evaluateTerm t))))

-- | Run an action with the passed ('Blob', @term@) pairs available for imports.
withModules :: (MonadAnalysis term value m, MonadEvaluator term value m) => [(Blob, term)] -> m a -> m a
withModules pairs = localModuleTable (const moduleTable)
  where moduleTable = Linker (Map.fromList (map (first (dropExtensions . blobPath)) pairs))

-- | An analysis performing concrete evaluation of @term@s to @value@s.
newtype Evaluation term value a = Evaluation { runEvaluation :: Evaluator term value (EvaluatorEffects term value) a }
  deriving (Applicative, Functor, Monad, MonadFail)

deriving instance MonadEvaluator term value (Evaluation term value)

instance ( Evaluatable (Base t)
         , FreeVariables t
         , MonadAddressable (LocationFor v) v (Evaluation t v)
         , MonadValue t v (Evaluation t v)
         , Recursive t
         , Semigroup (Cell (LocationFor v) v)
         )
         => MonadAnalysis t v (Evaluation t v) where
  analyzeTerm = eval
