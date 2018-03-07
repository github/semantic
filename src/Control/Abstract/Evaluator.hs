{-# LANGUAGE DataKinds, FunctionalDependencies, GeneralizedNewtypeDeriving, RankNTypes, StandaloneDeriving, UndecidableInstances #-}
module Control.Abstract.Evaluator where

import Prologue
import Control.Monad.Effect
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Linker
import Data.Abstract.FreeVariables (Name)
import Data.Set as Set
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
  -- | Set the global environment
  putGlobalEnv :: EnvironmentFor value -> m ()
  -- | Update the global environment.
  modifyGlobalEnv :: (EnvironmentFor value -> EnvironmentFor value) -> m ()

  -- | Scope the set of exported symbols to the global environment
  addExport :: (Name, Name) -> m ()
  getExports :: m (Set (Name, Name))
  setExports :: Set (Name, Name) -> m ()

  -- | Retrieve the local environment.
  askLocalEnv :: m (EnvironmentFor value)
  -- | Run an action with a locally-modified environment.
  localEnv :: (EnvironmentFor value -> EnvironmentFor value) -> m a -> m a

  -- | Retrieve the heap.
  getStore :: m (StoreFor value)
  -- | Update the heap.
  modifyStore :: (StoreFor value -> StoreFor value) -> m ()

  -- | Retrieve the table of evaluated modules.
  getModuleTable :: m (Linker (EnvironmentFor value))
  -- | Update the table of evaluated modules.
  modifyModuleTable :: (Linker (EnvironmentFor value) -> Linker (EnvironmentFor value)) -> m ()

  -- | Retrieve the table of unevaluated modules.
  askModuleTable :: m (Linker term)
  -- | Run an action with a locally-modified table of unevaluated modules.
  localModuleTable :: (Linker term -> Linker term) -> m a -> m a

instance Members '[ Fail
                  , Reader (EnvironmentFor value)
                  , State  (Set (Name, Name))
                  , State  (EnvironmentFor value)
                  , State  (StoreFor value)
                  , Reader (Linker term)
                  , State  (Linker (EnvironmentFor value))
                  ] effects
         => MonadEvaluator term value (Evaluator effects term value) where
  getGlobalEnv = Evaluator get
  putGlobalEnv = Evaluator . put
  modifyGlobalEnv f = Evaluator (modify f)

  addExport = Evaluator . modify . Set.insert
  getExports = Evaluator get
  setExports = Evaluator . put

  askLocalEnv = Evaluator ask
  localEnv f a = Evaluator (local f (runEvaluator a))

  getStore = Evaluator get
  modifyStore f = Evaluator (modify f)

  getModuleTable = Evaluator get
  modifyModuleTable f = Evaluator (modify f)

  askModuleTable = Evaluator ask
  localModuleTable f a = Evaluator (local f (runEvaluator a))


-- | An evaluator of @term@s to @value@s, producing incremental results of type @a@ using a list of @effects@.
newtype Evaluator effects term value a = Evaluator { runEvaluator :: Eff effects a }
  deriving (Applicative, Functor, Monad)

deriving instance Member Fail effects => MonadFail (Evaluator effects term value)
deriving instance Member NonDetEff effects => Alternative (Evaluator effects term value)
deriving instance Member Fresh effects => MonadFresh (Evaluator effects term value)
