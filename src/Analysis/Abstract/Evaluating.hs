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
import Data.Abstract.ModuleTable
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
            , MonadAddressable (LocationFor value) (Evaluating term value (EvaluatingEffects term value '[]))
            , MonadValue value (Evaluating term value (EvaluatingEffects term value '[]))
            , Recursive term
            )
         => term
         -> Final (EvaluatingEffects term value '[]) value
evaluate = run @(Evaluating term value) @(EvaluatingEffects term value '[]) . evaluateModule

-- | Evaluate terms and an entry point to a value.
evaluates :: forall value term
          .  ( Evaluatable (Base term)
             , FreeVariables term
             , MonadAddressable (LocationFor value) (Evaluating term value (EvaluatingEffects term value '[]))
             , MonadValue value (Evaluating term value (EvaluatingEffects term value '[]))
             , Recursive term
             )
          => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
          -> (Blob, term)   -- Entrypoint
          -> Final (EvaluatingEffects term value '[]) value
evaluates pairs (_, t) = run @(Evaluating term value) @(EvaluatingEffects term value '[]) (withModules pairs (evaluateModule t))

-- | Run an action with the passed ('Blob', @term@) pairs available for imports.
withModules :: (MonadAnalysis m, MonadEvaluator m) => [(Blob, TermFor m)] -> m a -> m a
withModules pairs = localModuleTable (const moduleTable)
  where moduleTable = ModuleTable (Map.fromList (map (first (dropExtensions . blobPath)) pairs))

-- | An analysis evaluating @term@s to @value@s with a list of @effects@ using 'Evaluatable', and producing incremental results of type @a@.
newtype Evaluating term value effects a = Evaluating { runEvaluating :: Eff effects a }
  deriving (Applicative, Effectful, Functor, Monad)

deriving instance Member Fail      effects => MonadFail   (Evaluating term value effects)
deriving instance Member Fresh     effects => MonadFresh  (Evaluating term value effects)
deriving instance Member NonDetEff effects => Alternative (Evaluating term value effects)
deriving instance Member NonDetEff effects => MonadNonDet (Evaluating term value effects)

-- instance Effectful (Evaluating term value) where
--   lift = _

type EvaluatingEffects term value effects
  = Fail                          -- Failure with an error message
 ': Reader (EnvironmentFor value) -- Local environment (e.g. binding over a closure)
 ': State  (EnvironmentFor value) -- Global (imperative) environment
 ': State  (StoreFor value)       -- The heap
 ': Reader (ModuleTable term)     -- Cache of unevaluated modules
 ': State  (ModuleTable value)    -- Cache of evaluated modules
 ': effects

instance Members (EvaluatingEffects term value '[]) effects => MonadEvaluator (Evaluating term value effects) where
  type TermFor  (Evaluating term value effects) = term
  type ValueFor (Evaluating term value effects) = value

  getGlobalEnv = lift get
  modifyGlobalEnv f = lift (modify f)

  askLocalEnv = lift ask
  localEnv f a = lift (local f (lower a))

  getStore = lift get
  modifyStore f = lift (modify f)

  getModuleTable = lift get
  modifyModuleTable f = lift (modify f)

  askModuleTable = lift ask
  localModuleTable f a = lift (local f (lower a))

instance ( Evaluatable (Base term)
         , FreeVariables term
         , Members (EvaluatingEffects term value '[]) effects
         , MonadAddressable (LocationFor value) (Evaluating term value effects)
         , MonadValue value (Evaluating term value effects)
         , Recursive term
         )
         => MonadAnalysis (Evaluating term value effects) where
  analyzeTerm = eval
