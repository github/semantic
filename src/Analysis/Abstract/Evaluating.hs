{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, MultiParamTypeClasses, UndecidableInstances #-}
module Analysis.Abstract.Evaluating where

import Control.Abstract.Evaluator
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.NonDet
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
            , MonadAddressable (LocationFor value) (Evaluating term value (EvaluatorEffects term value))
            , MonadValue value (Evaluating term value (EvaluatorEffects term value))
            , Ord (LocationFor value)
            , Recursive term
            , Semigroup (CellFor value)
            )
         => term
         -> Final (EvaluatorEffects term value) value
evaluate = run @(EvaluatorEffects term value) . runEvaluator . runEvaluating . evaluateModule

-- | Evaluate terms and an entry point to a value.
evaluates :: forall value term
          .  ( Evaluatable (Base term)
             , FreeVariables term
             , MonadAddressable (LocationFor value) (Evaluating term value (EvaluatorEffects term value))
             , MonadValue value (Evaluating term value (EvaluatorEffects term value))
             , Ord (LocationFor value)
             , Recursive term
             , Semigroup (CellFor value)
             )
          => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
          -> (Blob, term)   -- Entrypoint
          -> Final (EvaluatorEffects term value) value
evaluates pairs (_, t) = run @(EvaluatorEffects term value) (runEvaluator (runEvaluating (withModules pairs (evaluateModule t))))

-- | Run an action with the passed ('Blob', @term@) pairs available for imports.
withModules :: (MonadAnalysis m, MonadEvaluator m) => [(Blob, TermFor m)] -> m a -> m a
withModules pairs = localModuleTable (const moduleTable)
  where moduleTable = Linker (Map.fromList (map (first (dropExtensions . blobPath)) pairs))

-- | An analysis performing concrete evaluation of @term@s to @value@s.
newtype Evaluating term value effects a = Evaluating { runEvaluating :: Evaluator term value effects a }
  deriving (Applicative, Functor, Effectful, Monad)

deriving instance Member Fail effects => MonadFail (Evaluating term value effects)
deriving instance Member Fresh effects => MonadFresh (Evaluating term value effects)
deriving instance Member NonDetEff effects => Alternative (Evaluating term value effects)
deriving instance Member NonDetEff effects => MonadNonDet (Evaluating term value effects)
deriving instance (Member Fail effects, MonadEvaluator (Evaluator term value effects), Ord (LocationFor value)) => MonadEvaluator (Evaluating term value effects)

instance ( Evaluatable (Base term)
         , FreeVariables term
         , Members (EvaluatorEffects term value) effects
         , MonadAddressable (LocationFor value) (Evaluating term value effects)
         , MonadValue value (Evaluating term value effects)
         , Recursive term
         , Semigroup (CellFor value)
         )
         => MonadAnalysis (Evaluating term value effects) where
  analyzeTerm = eval
