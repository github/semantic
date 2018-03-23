{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Evaluating
( type Evaluating
, require
, load
) where

import Control.Abstract.Evaluator
import Control.Monad.Effect
import Data.Abstract.Configuration
import qualified Data.Abstract.Environment as Env
import Data.Abstract.Environment (Environment)
import qualified Data.Abstract.Exports as Export
import Data.Abstract.Exports (Exports)
import Data.Abstract.Evaluatable
import Data.Abstract.Module
import Data.Abstract.ModuleTable
import Data.Abstract.Value
import qualified Data.ByteString.Char8 as BC
import qualified Data.IntMap as IntMap
import Prelude hiding (fail)
import Prologue hiding (throwError)

-- | Require/import another module by name and return it's environment and value.
--
-- Looks up the term's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: ( MonadAnalysis term value m
           , MonadThrow (EvaluateModule term) value m
           , MonadValue value m
           )
        => ModuleName
        -> m (EnvironmentFor value, value)
require name = getModuleTable >>= maybe (load name) pure . moduleTableLookup name

-- | Load another module by name and return it's environment and value.
--
-- Always loads/evaluates.
load :: forall term value m
     .  ( MonadAnalysis term value m
        , MonadThrow (EvaluateModule term) value m
        , MonadValue value m
        )
     => ModuleName
     -> m (EnvironmentFor value, value)
load name = askModuleTable >>= maybe notFound evalAndCache . moduleTableLookup name
  where
    notFound = fail ("cannot load module: " <> show name)

    evalAndCache []     = (,) <$> pure mempty <*> unit
    evalAndCache [x]    = evalAndCache' x
    evalAndCache (x:xs) = do
      (env, _) <- evalAndCache' x
      (env', v') <- evalAndCache xs
      pure (env <> env', v')

    evalAndCache' x = do
      v <- throwException (EvaluateModule x) :: m value
      env <- filterEnv <$> getExports <*> getEnv
      modifyModuleTable (moduleTableInsert name (env, v))
      pure (env, v)

    -- TODO: If the set of exports is empty because no exports have been
    -- defined, do we export all terms, or no terms? This behavior varies across
    -- languages. We need better semantics rather than doing it ad-hoc.
    filterEnv :: Exports l a -> Environment l a -> Environment l a
    filterEnv ports env
      | Export.null ports = env
      | otherwise = Export.toEnvironment ports <> Env.overwrite (Export.aliases ports) env

-- | An analysis evaluating @term@s to @value@s with a list of @effects@ using 'Evaluatable', and producing incremental results of type @a@.
newtype Evaluating term value effects a = Evaluating (Eff effects a)
  deriving (Applicative, Functor, Effectful, Monad)

deriving instance Member Fail      effects => MonadFail   (Evaluating term value effects)
deriving instance Member Fresh     effects => MonadFresh  (Evaluating term value effects)
deriving instance Member NonDet effects => Alternative (Evaluating term value effects)
deriving instance Member NonDet effects => MonadNonDet (Evaluating term value effects)

-- | Effects necessary for evaluating (whether concrete or abstract).
type EvaluatingEffects term value
  = '[ Resumable Prelude.String value
     , Fail                                        -- Failure with an error message
     , Resumable (EvaluateModule term) value       -- Requests to evaluate a module in the outermost analysis
     , Reader [Module term]                        -- The stack of currently-evaluating modules.
     , State  (EnvironmentFor value)               -- Environments (both local and global)
     , State  (HeapFor value)                      -- The heap
     , Reader (ModuleTable [Module term])                 -- Cache of unevaluated modules
     , State  (ModuleTable (EnvironmentFor value, value)) -- Cache of evaluated modules
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

instance Members '[Reader (ModuleTable [Module term]), State (ModuleTable (EnvironmentFor value, value))] effects => MonadModuleTable term value (Evaluating term value effects) where
  getModuleTable = raise get
  putModuleTable = raise . put

  askModuleTable = raise ask
  localModuleTable f a = raise (local f (lower a))

instance Members (EvaluatingEffects term value) effects => MonadEvaluator term value (Evaluating term value effects) where
  getConfiguration term = Configuration term mempty <$> getEnv <*> getHeap

  askModuleStack = raise ask

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

  analyzeTerm term = resumeException @value (eval term) (\yield exc -> string (BC.pack exc) >>= yield)

  analyzeModule m = pushModule (subterm <$> m) (subtermValue (moduleBody m))

pushModule :: Member (Reader [Module term]) effects => Module term -> Evaluating term value effects a -> Evaluating term value effects a
pushModule m = raise . local (m :) . lower
