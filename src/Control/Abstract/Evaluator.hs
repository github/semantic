{-# LANGUAGE DataKinds, FunctionalDependencies, RankNTypes, UndecidableInstances #-}
module Control.Abstract.Evaluator where

import Control.Applicative
import Control.Monad.Effect
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Linker
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

instance MonadEvaluator term value (Evaluator effects term value) where
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


newtype Evaluator effects term value a
  = Evaluator
    { runEvaluator :: Members '[ Fail
                               , Reader (EnvironmentFor value)
                               , State  (EnvironmentFor value)
                               , State  (StoreFor value)
                               , Reader (Linker term)
                               , State  (Linker value)
                               ] effects
                   => Eff effects a
    }


instance Functor (Evaluator effects term value) where
  fmap f (Evaluator run) = Evaluator (fmap f run)

instance Applicative (Evaluator effects term value) where
  pure = Evaluator . pure

  Evaluator runF <*> Evaluator runA = Evaluator (runF <*> runA)

instance Member NonDetEff effects => Alternative (Evaluator effects term value) where
  empty = Evaluator empty

  Evaluator runA <|> Evaluator runB = Evaluator (runA <|> runB)

instance Monad (Evaluator effects term value) where
  return = pure

  Evaluator runA >>= f = Evaluator (runA >>= runEvaluator . f)

instance MonadFail (Evaluator effects term value) where
  fail s = Evaluator (fail s)

instance Member Fresh effects => MonadFresh (Evaluator effects term value) where
  fresh = Evaluator fresh

  reset t = Evaluator (reset t)
