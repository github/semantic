{-# LANGUAGE DataKinds, DefaultSignatures, FunctionalDependencies, GeneralizedNewtypeDeriving, RankNTypes, StandaloneDeriving, UndecidableInstances #-}
module Control.Abstract.Evaluator where

import Control.Applicative
import Control.Monad.Effect
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.NonDet
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Configuration
import Data.Abstract.Linker
import Data.Abstract.Live
import Data.Abstract.Value
import Prelude hiding (fail)

-- | A 'Monad' providing the basic essentials for evaluation.
--
--   These presently include:
--   - environments binding names to addresses
--   - a heap mapping addresses to (possibly sets of) values
--   - tables of modules available for import
class MonadFail m => MonadEvaluator term value m | m -> term, m -> value where
  -- | Retrieve the global environment.
  getGlobalEnv :: m (EnvironmentFor value)
  -- | Update the global environment.
  modifyGlobalEnv :: (EnvironmentFor value -> EnvironmentFor value) -> m ()

  -- | Retrieve the local environment.
  askLocalEnv :: m (EnvironmentFor value)
  -- | Run an action with a locally-modified environment.
  localEnv :: (EnvironmentFor value -> EnvironmentFor value) -> m a -> m a

  -- | Retrieve the heap.
  getStore :: m (StoreFor value)
  -- | Update the heap.
  modifyStore :: (StoreFor value -> StoreFor value) -> m ()

  -- | Retrieve the table of evaluated modules.
  getModuleTable :: m (Linker value)
  -- | Update the table of evaluated modules.
  modifyModuleTable :: (Linker value -> Linker value) -> m ()

  -- | Retrieve the table of unevaluated modules.
  askModuleTable :: m (Linker term)
  -- | Run an action with a locally-modified table of unevaluated modules.
  localModuleTable :: (Linker term -> Linker term) -> m a -> m a

  -- | Retrieve the current root set.
  askRoots :: Ord (LocationFor value) => m (Live (LocationFor value) value)
  askRoots = pure mempty

  -- | Get the current 'Configuration' with a passed-in term.
  getConfiguration :: Ord (LocationFor value) => term -> m (Configuration (LocationFor value) term value)
  getConfiguration term = Configuration term <$> askRoots <*> askLocalEnv <*> getStore

type EvaluatorEffects term value
  = '[ Fail
     , Reader (EnvironmentFor value)
     , State  (EnvironmentFor value)
     , State  (StoreFor value)
     , Reader (Linker term)
     , State  (Linker value)
     ]

instance Members (EvaluatorEffects term value) effects => MonadEvaluator term value (Evaluator effects term value) where
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

putStore :: MonadEvaluator t value m => StoreFor value -> m ()
putStore = modifyStore . const

-- | An evaluator of @term@s to @value@s, producing incremental results of type @a@ using a list of @effects@.
newtype Evaluator effects term value a = Evaluator { runEvaluator :: Eff effects a }
  deriving (Applicative, Functor, Monad)

deriving instance Member Fail effects => MonadFail (Evaluator effects term value)
deriving instance Member NonDetEff effects => Alternative (Evaluator effects term value)
deriving instance Member NonDetEff effects => MonadNonDet (Evaluator effects term value)
deriving instance Member Fresh effects => MonadFresh (Evaluator effects term value)
