{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Evaluating
( type Evaluating
, evaluate
) where

import Control.Abstract.Evaluator
import Control.Monad.Effect
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Configuration
import qualified Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable
import Data.Abstract.Module
import Data.Abstract.ModuleTable
import Data.Abstract.Value
import qualified Data.IntMap as IntMap
import Prelude hiding (fail)
import Prologue

-- | Evaluate a term to a value.
evaluate :: forall value term effects
         .  ( effects ~ RequiredEffects term value (Evaluating term value effects)
            , Evaluatable (Base term)
            , FreeVariables term
            , MonadAddressable (LocationFor value) value (Evaluating term value effects)
            , MonadValue value (Evaluating term value effects)
            , Recursive term
            , Show (LocationFor value)
            )
         => term
         -> Final effects value
evaluate = runAnalysis @(Evaluating term value) . evaluateModule


-- | An analysis evaluating @term@s to @value@s with a list of @effects@ using 'Evaluatable', and producing incremental results of type @a@.
newtype Evaluating term value effects a = Evaluating (Eff effects a)
  deriving (Applicative, Functor, Effectful, Monad)

deriving instance Member Fail      effects => MonadFail   (Evaluating term value effects)
deriving instance Member Fresh     effects => MonadFresh  (Evaluating term value effects)
deriving instance Member NonDetEff effects => Alternative (Evaluating term value effects)
deriving instance Member NonDetEff effects => MonadNonDet (Evaluating term value effects)

-- | Effects necessary for evaluating (whether concrete or abstract).
type EvaluatingEffects term value
  = '[ Fail                                        -- Failure with an error message
     , State  (EnvironmentFor value)               -- Environments (both local and global)
     , State  (HeapFor value)                      -- The heap
     , Reader (ModuleTable [Module term])          -- Cache of unevaluated modules
     , State  (ModuleTable (EnvironmentFor value)) -- Cache of evaluated modules
     , State  (ExportsFor value)                   -- Exports (used to filter environments when they are imported)
     , State  (IntMap.IntMap term)                 -- For jumps
     ]

instance Members '[Fail, State (IntMap.IntMap term)] effects => MonadControl term (Evaluating term value effects) where
  label term = do
    m <- raise get
    let i = IntMap.size m
    raise (put (IntMap.insert i term m))
    pure i

  goto label = IntMap.lookup label <$> raise get >>= maybe (fail ("unknown label: " <> show label)) pure

instance Members '[State (ExportsFor value), State (EnvironmentFor value)] effects => MonadEnvironment value (Evaluating term value effects) where
  getEnv = raise get
  putEnv = raise . put
  withEnv s = raise . localState s . lower

  getExports = raise get
  putExports = raise . put
  withExports s = raise . localState s . lower

  localEnv f a = do
    modifyEnv (f . Env.push)
    result <- a
    result <$ modifyEnv Env.pop

instance Member (State (HeapFor value)) effects => MonadHeap value (Evaluating term value effects) where
  getHeap = raise get
  putHeap = raise . put

instance Members '[Reader (ModuleTable [Module term]), State (ModuleTable (EnvironmentFor value))] effects => MonadModuleTable term value (Evaluating term value effects) where
  getModuleTable = raise get
  putModuleTable = raise . put

  askModuleTable = raise ask
  localModuleTable f a = raise (local f (lower a))

instance Members (EvaluatingEffects term value) effects => MonadEvaluator term value (Evaluating term value effects) where
  getConfiguration term = Configuration term mempty <$> getEnv <*> getHeap

instance ( Evaluatable (Base term)
         , FreeVariables term
         , Members (EvaluatingEffects term value) effects
         , MonadAddressable (LocationFor value) value (Evaluating term value effects)
         , MonadValue value (Evaluating term value effects)
         , Recursive term
         , Show (LocationFor value)
         )
         => MonadAnalysis term value (Evaluating term value effects) where
  type RequiredEffects term value (Evaluating term value effects) = EvaluatingEffects term value

  analyzeTerm = eval
