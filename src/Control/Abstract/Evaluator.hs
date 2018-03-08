{-# LANGUAGE ConstrainedClassMethods, DataKinds, DefaultSignatures, GeneralizedNewtypeDeriving, RankNTypes, StandaloneDeriving, TypeFamilies, UndecidableInstances #-}
module Control.Abstract.Evaluator where

import Control.Applicative
import Control.Effect
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

type family TermFor (m :: * -> *)
type family ValueFor (m :: * -> *)

-- | A 'Monad' providing the basic essentials for evaluation.
--
--   These presently include:
--   - environments binding names to addresses
--   - a heap mapping addresses to (possibly sets of) values
--   - tables of modules available for import
class MonadFail m => MonadEvaluator m where
  -- | Retrieve the global environment.
  getGlobalEnv :: m (EnvironmentFor (ValueFor m))
  -- | Update the global environment.
  modifyGlobalEnv :: (EnvironmentFor (ValueFor m) -> EnvironmentFor (ValueFor m)) -> m ()

  -- | Retrieve the local environment.
  askLocalEnv :: m (EnvironmentFor (ValueFor m))
  -- | Run an action with a locally-modified environment.
  localEnv :: (EnvironmentFor (ValueFor m) -> EnvironmentFor (ValueFor m)) -> m a -> m a

  -- | Retrieve the heap.
  getStore :: m (StoreFor (ValueFor m))
  -- | Update the heap.
  modifyStore :: (StoreFor (ValueFor m) -> StoreFor (ValueFor m)) -> m ()

  -- | Retrieve the table of evaluated modules.
  getModuleTable :: m (Linker (ValueFor m))
  -- | Update the table of evaluated modules.
  modifyModuleTable :: (Linker (ValueFor m) -> Linker (ValueFor m)) -> m ()

  -- | Retrieve the table of unevaluated modules.
  askModuleTable :: m (Linker (TermFor m))
  -- | Run an action with a locally-modified table of unevaluated modules.
  localModuleTable :: (Linker (TermFor m) -> Linker (TermFor m)) -> m a -> m a

  -- | Retrieve the current root set.
  askRoots :: Ord (LocationFor (ValueFor m)) => m (Live (LocationFor (ValueFor m)) (ValueFor m))
  askRoots = pure mempty

  -- | Get the current 'Configuration' with a passed-in term.
  getConfiguration :: Ord (LocationFor (ValueFor m)) => term -> m (Configuration (LocationFor (ValueFor m)) term (ValueFor m))
  getConfiguration term = Configuration term <$> askRoots <*> askLocalEnv <*> getStore

type EvaluatorEffects term value
  = '[ Fail                          -- Failure with an error message
     , Reader (EnvironmentFor value) -- Local environment (e.g. binding over a closure)
     , State  (EnvironmentFor value) -- Global (imperative) environment
     , State  (StoreFor value)       -- The heap
     , Reader (Linker term)          -- Cache of unevaluated modules
     , State  (Linker value)         -- Cache of evaluated modules
     ]

type instance TermFor (Evaluator term value effects) = term
type instance ValueFor (Evaluator term value effects) = value
instance (Ord (LocationFor value), Members (EvaluatorEffects term value) effects) => MonadEvaluator (Evaluator term value effects) where
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

putStore :: MonadEvaluator m => StoreFor (ValueFor m) -> m ()
putStore = modifyStore . const

-- | An evaluator of @term@s to @value@s, producing incremental results of type @a@ using a list of @effects@.
newtype Evaluator term value effects a = Evaluator { runEvaluator :: Eff effects a }
  deriving (Applicative, Functor, LiftEffect, Monad)

deriving instance Member Fail effects => MonadFail (Evaluator term value effects)
deriving instance Member NonDetEff effects => Alternative (Evaluator term value effects)
deriving instance Member NonDetEff effects => MonadNonDet (Evaluator term value effects)
deriving instance Member Fresh effects => MonadFresh (Evaluator term value effects)
