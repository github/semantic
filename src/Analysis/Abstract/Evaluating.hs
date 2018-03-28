{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, StandaloneDeriving, TypeFamilies, UndecidableInstances #-}
module Analysis.Abstract.Evaluating
( Evaluating
, EvaluatingState(..)
) where

import           Control.Abstract.Analysis
import           Control.Monad.Effect
import           Data.Abstract.Configuration
import qualified Data.Abstract.Environment as Env
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import           Data.Abstract.ModuleTable
import qualified Data.IntMap as IntMap
import           Lens.Micro
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
     , Fail                                -- Failure with an error message
     , Reader [Module term]                -- The stack of currently-evaluating modules.
     , Reader (ModuleTable [Module term])  -- Cache of unevaluated modules
     , Reader (EnvironmentFor value)       -- Default environment used as a fallback in lookupEnv
     , State  (EvaluatingState term value) -- Environment, heap, modules, exports, and jumps.
     ]

data EvaluatingState term value = EvaluatingState
  { environment :: EnvironmentFor value
  , heap        :: HeapFor value
  , modules     :: ModuleTable (EnvironmentFor value, value)
  , exports     :: ExportsFor value
  , jumps       :: IntMap.IntMap term
  }

deriving instance (Eq (CellFor value), Eq (LocationFor value), Eq term, Eq value) => Eq (EvaluatingState term value)
deriving instance (Ord (CellFor value), Ord (LocationFor value), Ord term, Ord value) => Ord (EvaluatingState term value)
deriving instance (Show (CellFor value), Show (LocationFor value), Show term, Show value) => Show (EvaluatingState term value)

instance (Ord (LocationFor value), Semigroup (CellFor value)) => Semigroup (EvaluatingState term value) where
  EvaluatingState e1 h1 m1 x1 j1 <> EvaluatingState e2 h2 m2 x2 j2 = EvaluatingState (e1 <> e2) (h1 <> h2) (m1 <> m2) (x1 <> x2) (j1 <> j2)

instance (Ord (LocationFor value), Semigroup (CellFor value)) => Monoid (EvaluatingState term value) where
  mempty = EvaluatingState mempty mempty mempty mempty mempty
  mappend = (<>)

_environment :: Lens' (EvaluatingState term value) (EnvironmentFor value)
_environment = lens environment (\ s e -> s {environment = e})

_heap :: Lens' (EvaluatingState term value) (HeapFor value)
_heap = lens heap (\ s h -> s {heap = h})

_modules :: Lens' (EvaluatingState term value) (ModuleTable (EnvironmentFor value, value))
_modules = lens modules (\ s m -> s {modules = m})

_exports :: Lens' (EvaluatingState term value) (ExportsFor value)
_exports = lens exports (\ s e -> s {exports = e})

_jumps :: Lens' (EvaluatingState term value) (IntMap.IntMap term)
_jumps = lens jumps (\ s j -> s {jumps = j})


(.=) :: Member (State (EvaluatingState term value)) effects => ASetter (EvaluatingState term value) (EvaluatingState term value) a b -> b -> Evaluating term value effects ()
lens .= val = raise (modify' (lens .~ val))

view :: Member (State (EvaluatingState term value)) effects => Getting a (EvaluatingState term value) a -> Evaluating term value effects a
view lens = raise (gets (^. lens))

localEvaluatingState :: Member (State (EvaluatingState term value)) effects => (EvaluatingState term value -> EvaluatingState term value) -> Evaluating term value effects a -> Evaluating term value effects a
localEvaluatingState f = raise . localState f . lower


instance Members '[Fail, State (EvaluatingState term value)] effects => MonadControl term (Evaluating term value effects) where
  label term = do
    m <- view _jumps
    let i = IntMap.size m
    _jumps .= IntMap.insert i term m
    pure i

  goto label = IntMap.lookup label <$> view _jumps >>= maybe (fail ("unknown label: " <> show label)) pure

instance Members '[ State (EvaluatingState term value)
                  , Reader (EnvironmentFor value)
                  ] effects
      => MonadEnvironment value (Evaluating term value effects) where
  getEnv = view _environment
  putEnv = (_environment .=)
  withEnv s = localEvaluatingState (_environment .~ s)

  defaultEnvironment = raise ask
  withDefaultEnvironment e = raise . local (const e) . lower

  getExports = view _exports
  putExports = (_exports .=)
  withExports s = localEvaluatingState (_exports .~ s)

  localEnv f a = do
    modifyEnv (f . Env.push)
    result <- a
    result <$ modifyEnv Env.pop

instance Member (State (EvaluatingState term value)) effects => MonadHeap value (Evaluating term value effects) where
  getHeap = view _heap
  putHeap = (_heap .=)

instance Members '[Reader (ModuleTable [Module term]), State (EvaluatingState term value)] effects => MonadModuleTable term value (Evaluating term value effects) where
  getModuleTable = view _modules
  putModuleTable = (_modules .=)

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
