{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, MultiParamTypeClasses, UndecidableInstances #-}
module Analysis.Abstract.Evaluating where

import Control.Abstract.Evaluator
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.NonDet
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Evaluatable
import Data.Abstract.Linker
import Data.Abstract.Value
import Data.Blob
import qualified Data.Map as Map
import Prelude hiding (fail)
import Prologue
import System.FilePath.Posix

-- | Evaluate a term to a value.
evaluate :: forall value term
         .  ( Evaluatable (Base term)
            , FreeVariables term
            , MonadAddressable (LocationFor value) (Evaluating term value (EvaluatingEffects term value))
            , MonadValue value (Evaluating term value (EvaluatingEffects term value))
            , Ord (LocationFor value)
            , Recursive term
            , Semigroup (CellFor value)
            )
         => term
         -> Final (EvaluatingEffects term value) value
evaluate = run @(EvaluatingEffects term value) . runEvaluator . runEvaluating . evaluateModule

-- | Evaluate terms and an entry point to a value.
evaluates :: forall value term
          .  ( Evaluatable (Base term)
             , FreeVariables term
             , MonadAddressable (LocationFor value) (Evaluating term value (EvaluatingEffects term value))
             , MonadValue value (Evaluating term value (EvaluatingEffects term value))
             , Ord (LocationFor value)
             , Recursive term
             , Semigroup (CellFor value)
             )
          => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
          -> (Blob, term)   -- Entrypoint
          -> Final (EvaluatingEffects term value) value
evaluates pairs (_, t) = run @(EvaluatingEffects term value) (runEvaluator (runEvaluating (withModules pairs (evaluateModule t))))

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
         , Members (EvaluatingEffects term value) effects
         , MonadAddressable (LocationFor value) (Evaluating term value effects)
         , MonadValue value (Evaluating term value effects)
         , Recursive term
         , Semigroup (CellFor value)
         )
         => MonadAnalysis (Evaluating term value effects) where
  analyzeTerm = eval

type EvaluatingEffects term value
  = '[ Fail                          -- Failure with an error message
     , Reader (EnvironmentFor value) -- Local environment (e.g. binding over a closure)
     , State  (EnvironmentFor value) -- Global (imperative) environment
     , State  (StoreFor value)       -- The heap
     , Reader (Linker term)          -- Cache of unevaluated modules
     , State  (Linker value)         -- Cache of evaluated modules
     ]

instance (Ord (LocationFor value), Members (EvaluatingEffects term value) effects) => MonadEvaluator (Evaluator term value effects) where
  type TermFor (Evaluator term value effects) = term
  type ValueFor (Evaluator term value effects) = value

  getGlobalEnv = Evaluator get
  modifyGlobalEnv f = Evaluator (modify f)

  askLocalEnv = Evaluator ask
  localEnv f a = Evaluator (local f (runEvaluator a))

  getStore = Evaluator get
  modifyStore f = Evaluator (modify f)

  getModuleTable = Evaluator get
  modifyModuleTable f = Evaluator (modify f)

  askModuleTable = Evaluator ask
  localModuleTable f a = Evaluator (local f (runEvaluator a))

-- | An evaluator of @term@s to @value@s, producing incremental results of type @a@ using a list of @effects@.
newtype Evaluator term value effects a = Evaluator { runEvaluator :: Eff effects a }
  deriving (Applicative, Functor, Effectful, Monad)

deriving instance Member Fail effects => MonadFail (Evaluator term value effects)
deriving instance Member NonDetEff effects => Alternative (Evaluator term value effects)
deriving instance Member NonDetEff effects => MonadNonDet (Evaluator term value effects)
deriving instance Member Fresh effects => MonadFresh (Evaluator term value effects)
