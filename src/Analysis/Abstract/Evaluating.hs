{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, StandaloneDeriving, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module Analysis.Abstract.Evaluating
( Evaluating
, findValue
, findEnv
, findHeap
) where

import           Control.Abstract.Analysis
import           Control.Monad.Effect
import           Data.Abstract.Configuration
import qualified Data.Abstract.Environment as Env
import           Data.Abstract.Evaluatable
import           Data.Abstract.Heap
import           Data.Abstract.Module
import           Data.Abstract.ModuleTable
import qualified Data.IntMap as IntMap
import qualified Data.Map.Monoidal as Monoidal
import           Lens.Micro
import           Lens.Micro.TH
import           Prelude hiding (fail)
import           Prologue

-- | An analysis evaluating @term@s to @value@s with a list of @effects@ using 'Evaluatable', and producing incremental results of type @a@.
newtype Evaluating term value effects a = Evaluating (Eff effects a)
  deriving (Applicative, Functor, Effectful, Monad)

deriving instance Member Fail      effects => MonadFail   (Evaluating term value effects)
deriving instance Member Fresh     effects => MonadFresh  (Evaluating term value effects)
deriving instance Member NonDet    effects => Alternative (Evaluating term value effects)
deriving instance Member NonDet    effects => MonadNonDet (Evaluating term value effects)

-- | Effects necessary for evaluating (whether concrete or abstract).
type EvaluatingEffects term value
  = '[ Resumable (ValueExc value)
     , Resumable (Unspecialized value)
     , Fail                                               -- Failure with an error message
     , Reader [Module term]                               -- The stack of currently-evaluating modules.
     , State  (EnvironmentFor value)                      -- Environments (both local and global)
     , State  (HeapFor value)                             -- The heap
     , Reader (ModuleTable [Module term])                 -- Cache of unevaluated modules
     , Reader (EnvironmentFor value)                      -- Default environment used as a fallback in lookupEnv
     , State  (ModuleTable (EnvironmentFor value, value)) -- Cache of evaluated modules
     , State  (ExportsFor value)                          -- Exports (used to filter environments when they are imported)
     , State  (IntMap.IntMap term)                        -- For jumps
     ]

data EvaluatingState term value = EvaluatingState
  { _environment :: EnvironmentFor value
  , _heap        :: HeapFor value
  , _modules     :: ModuleTable (EnvironmentFor value, value)
  , _exports     :: ExportsFor value
  , _jumps       :: IntMap.IntMap term
  }

makeLenses ''EvaluatingState

(.=) :: (Effectful m, Member (State s) effects) => ASetter s s a b -> b -> m effects ()
lens .= val = raise (modify (lens .~ val))


-- | Find the value in the 'Final' result of running.
findValue :: (effects ~ RequiredEffects term value (Evaluating term value effects))
          => Final effects value -> Either Prelude.String (Either (SomeExc (Unspecialized value)) (Either (SomeExc (ValueExc value)) value))
findValue (((((v, _), _), _), _), _) = v

-- | Find the 'Environment' in the 'Final' result of running.
findEnv :: (effects ~ RequiredEffects term value (Evaluating term value effects))
        => Final effects value -> EnvironmentFor value
findEnv (((((_, env), _), _), _), _) = env

-- | Find the 'Heap' in the 'Final' result of running.
findHeap :: (effects ~ RequiredEffects term value (Evaluating term value effects))
         => Final effects value -> Monoidal.Map (LocationFor value) (CellFor value)
findHeap (((((_, _), Heap heap), _), _), _) = heap


instance Members '[Fail, State (IntMap.IntMap term)] effects => MonadControl term (Evaluating term value effects) where
  label term = do
    m <- raise get
    let i = IntMap.size m
    raise (put (IntMap.insert i term m))
    pure i

  goto label = IntMap.lookup label <$> raise get >>= maybe (fail ("unknown label: " <> show label)) pure

instance Members '[ State (ExportsFor value)
                  , State (EnvironmentFor value)
                  , Reader (EnvironmentFor value)
                  ] effects => MonadEnvironment value (Evaluating term value effects) where
  getEnv = raise get
  putEnv = raise . put
  withEnv s = raise . localState s . lower

  defaultEnvironment = raise ask
  withDefaultEnvironment e = raise . local (const e) . lower

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

instance ( Members (EvaluatingEffects term value) effects
         , MonadValue value (Evaluating term value effects)
         )
         => MonadAnalysis term value (Evaluating term value effects) where
  type RequiredEffects term value (Evaluating term value effects) = EvaluatingEffects term value

  analyzeTerm = id

  analyzeModule eval m = pushModule (subterm <$> m) (eval m)

pushModule :: Member (Reader [Module term]) effects => Module term -> Evaluating term value effects a -> Evaluating term value effects a
pushModule m = raise . local (m :) . lower
