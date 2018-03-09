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
import Data.List.Split (splitWhen)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import Prelude hiding (fail)
import Prologue
import System.FilePath.Posix

-- | Evaluate a term to a value.
evaluate :: forall value term
         .  ( Evaluatable (Base term)
            , FreeVariables term
            , MonadAddressable (LocationFor value) term value (EvaluatingEffects term value) Evaluating
            , MonadValue term value (Evaluating term value (EvaluatingEffects term value))
            , Recursive term
            )
         => term
         -> Final (EvaluatingEffects term value) value
evaluate = run @(Evaluating term value) @(EvaluatingEffects term value) . evaluateModule

-- | Evaluate terms and an entry point to a value.
evaluates :: forall value term
          .  ( Evaluatable (Base term)
             , FreeVariables term
             , MonadAddressable (LocationFor value) term value (EvaluatingEffects term value) Evaluating
             , MonadValue term value (Evaluating term value (EvaluatingEffects term value))
             , Recursive term
             )
          => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
          -> (Blob, term)   -- Entrypoint
          -> Final (EvaluatingEffects term value) value
evaluates pairs (b, t) = run @(Evaluating term value) @(EvaluatingEffects term value) (withModules b pairs (evaluateModule t))

-- | Run an action with the passed ('Blob', @term@) pairs available for imports.
withModules :: MonadAnalysis term value effects m => Blob -> [(Blob, term)] -> m term value effects a -> m term value effects a
withModules Blob{..} pairs = localModuleTable (const moduleTable)
  where
    moduleTable = ModuleTable (Map.fromList (map (first moduleName) pairs))
    rootDir = dropFileName blobPath
    moduleName Blob{..} = toName (dropExtensions (makeRelative rootDir blobPath))
    toName str = qualifiedName (fmap BC.pack (splitWhen (== pathSeparator) str))

-- | An analysis evaluating @term@s to @value@s with a list of @effects@ using 'Evaluatable', and producing incremental results of type @a@.
newtype Evaluating term value effects a = Evaluating { runEvaluating :: Eff effects a }
  deriving (Applicative, Functor, Effectful, Monad)



deriving instance Member Fail      effects => MonadFail   (Evaluating term value effects)
deriving instance Member Fresh     effects => MonadFresh  (Evaluating term value effects)
deriving instance Member NonDetEff effects => Alternative (Evaluating term value effects)
deriving instance Member NonDetEff effects => MonadNonDet (Evaluating term value effects)

type EvaluatingEffects term value
  = '[ Fail                                        -- Failure with an error message
     , Reader (EnvironmentFor value)               -- Local environment (e.g. binding over a closure)
     , State  (EnvironmentFor value)               -- Global (imperative) environment
     , State  (StoreFor value)                     -- The heap
     , Reader (ModuleTable term)                   -- Cache of unevaluated modules
     , State  (ModuleTable (EnvironmentFor value)) -- Cache of evaluated modules
     ]

instance Members (EvaluatingEffects term value) effects => MonadEvaluator term value effects Evaluating where
  getGlobalEnv = lift get
  putGlobalEnv = lift . put
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
         , Members (EvaluatingEffects term value) effects
         , MonadAddressable (LocationFor value) term value effects Evaluating
         , MonadValue term value (Evaluating term value effects)
         , Recursive term
         )
         => MonadAnalysis term value effects Evaluating where
  type RequiredEffects term value Evaluating = EvaluatingEffects term value
  analyzeTerm = eval
