{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, TypeFamilies, UndecidableInstances, ScopedTypeVariables #-}
module Analysis.Abstract.Evaluating
( Evaluating
, EvaluatingState(..)
, State
) where

import           Control.Abstract.Analysis
import           Control.Monad.Effect
import           Data.Abstract.Address
import           Data.Abstract.Configuration
import           Data.Abstract.Environment as Env
import           Data.Abstract.Evaluatable
import           Data.Abstract.Exports
import           Data.Abstract.Heap
import           Data.Abstract.Module
import           Data.Abstract.ModuleTable
import           Data.Abstract.Origin
import           Data.Empty
import qualified Data.IntMap as IntMap
import           Lens.Micro
import           Prelude hiding (fail)
import           Prologue

-- | An analysis evaluating @term@s to @value@s with a list of @effects@ using 'Evaluatable', and producing incremental results of type @a@.
newtype Evaluating location term value effects a = Evaluating (Eff effects a)
  deriving (Applicative, Functor, Effectful, Monad)

deriving instance Member Fail   effects => MonadFail   (Evaluating location term value effects)
deriving instance Member Fresh  effects => MonadFresh  (Evaluating location term value effects)
deriving instance Member NonDet effects => Alternative (Evaluating location term value effects)

-- | Effects necessary for evaluating (whether concrete or abstract).
type EvaluatingEffects location term value
  = '[ Exc (ControlThrow value)
     , Resumable (EvalError value)
     , Resumable (ResolutionError value)
     , Resumable (LoadError term value)
     , Resumable (ValueError location value)
     , Resumable (Unspecialized value)
     , Resumable (AddressError location value)
     , Fail                                         -- Failure with an error message
     , Fresh                                        -- For allocating new addresses and/or type variables.
     , Reader (SomeOrigin term)                     -- The current term’s origin.
     , Reader (ModuleTable [Module term])           -- Cache of unevaluated modules
     , Reader (Environment location value)          -- Default environment used as a fallback in lookupEnv
     , State  (EvaluatingState location term value) -- Environment, heap, modules, exports, and jumps.
     ]

data EvaluatingState location term value = EvaluatingState
  { environment :: Environment location value
  , heap        :: Heap location value
  , modules     :: ModuleTable (Environment location value, value)
  , loadStack   :: LoadStack
  , exports     :: Exports location value
  , jumps       :: IntMap.IntMap term
  , origin      :: SomeOrigin term
  }

deriving instance (Eq (Cell location value), Eq location, Eq term, Eq value, Eq (Base term ())) => Eq (EvaluatingState location term value)
deriving instance (Ord (Cell location value), Ord location, Ord term, Ord value, Ord (Base term ())) => Ord (EvaluatingState location term value)
deriving instance (Show (Cell location value), Show location, Show term, Show value, Show (Base term ())) => Show (EvaluatingState location term value)

instance (Ord location, Semigroup (Cell location value)) => Semigroup (EvaluatingState location term value) where
  EvaluatingState e1 h1 m1 l1 x1 j1 o1 <> EvaluatingState e2 h2 m2 l2 x2 j2 o2 = EvaluatingState (e1 <> e2) (h1 <> h2) (m1 <> m2) (l1 <> l2) (x1 <> x2) (j1 <> j2) (o1 <> o2)

instance (Ord location, Semigroup (Cell location value)) => Empty (EvaluatingState location term value) where
  empty = EvaluatingState mempty mempty mempty mempty mempty mempty mempty

_environment :: Lens' (EvaluatingState location term value) (Environment location value)
_environment = lens environment (\ s e -> s {environment = e})

_heap :: Lens' (EvaluatingState location term value) (Heap location value)
_heap = lens heap (\ s h -> s {heap = h})

_modules :: Lens' (EvaluatingState location term value) (ModuleTable (Environment location value, value))
_modules = lens modules (\ s m -> s {modules = m})

_loadStack :: Lens' (EvaluatingState location term value) LoadStack
_loadStack = lens loadStack (\ s l -> s {loadStack = l})

_exports :: Lens' (EvaluatingState location term value) (Exports location value)
_exports = lens exports (\ s e -> s {exports = e})

_jumps :: Lens' (EvaluatingState location term value) (IntMap.IntMap term)
_jumps = lens jumps (\ s j -> s {jumps = j})

_origin :: Lens' (EvaluatingState location term value) (SomeOrigin term)
_origin = lens origin (\ s o -> s {origin = o})


(.=) :: Member (State (EvaluatingState location term value)) effects => ASetter (EvaluatingState location term value) (EvaluatingState location term value) a b -> b -> Evaluating location term value effects ()
lens .= val = raise (modify' (lens .~ val))

view :: Member (State (EvaluatingState location term value)) effects => Getting a (EvaluatingState location term value) a -> Evaluating location term value effects a
view lens = raise (gets (^. lens))

localEvaluatingState :: Member (State (EvaluatingState location term value)) effects => Lens' (EvaluatingState location term value) prj -> (prj -> prj) -> Evaluating location term value effects a -> Evaluating location term value effects a
localEvaluatingState lens f action = do
  original <- view lens
  lens .= f original
  v <- action
  v <$ lens .= original


instance Members '[Fail, State (EvaluatingState location term value)] effects => MonadControl term (Evaluating location term value effects) where
  label term = do
    m <- view _jumps
    let i = IntMap.size m
    _jumps .= IntMap.insert i term m
    pure i

  goto label = IntMap.lookup label <$> view _jumps >>= maybe (fail ("unknown label: " <> show label)) pure

instance Members '[ State (EvaluatingState location term value)
                  , Reader (Environment location value)
                  ] effects
      => MonadEnvironment location value (Evaluating location term value effects) where
  getEnv = view _environment
  putEnv = (_environment .=)
  withEnv s = localEvaluatingState _environment (const s)

  defaultEnvironment = raise ask
  withDefaultEnvironment e = raise . local (const e) . lower

  getExports = view _exports
  putExports = (_exports .=)
  withExports s = localEvaluatingState _exports (const s)

  localEnv f a = do
    modifyEnv (f . Env.push)
    result <- a
    result <$ modifyEnv Env.pop

instance Member (State (EvaluatingState location term value)) effects
      => MonadHeap location value (Evaluating location term value effects) where
  getHeap = view _heap
  putHeap = (_heap .=)

instance Members '[ Reader (ModuleTable [Module term])
                  , State (EvaluatingState location term value)
                  , Reader (SomeOrigin term)
                  , Fail
                  ] effects
      => MonadModuleTable location term value (Evaluating location term value effects) where
  getModuleTable = view _modules
  putModuleTable = (_modules .=)

  askModuleTable = raise ask
  localModuleTable f a = raise (local f (lower a))

  getLoadStack = view _loadStack
  putLoadStack = (_loadStack .=)

  currentModule = do
    o <- raise ask
    maybeFail "unable to get currentModule" $ withSomeOrigin (originModule @term) o

instance Members (EvaluatingEffects location term value) effects
      => MonadEvaluator location term value (Evaluating location term value effects) where
  getConfiguration term = Configuration term mempty <$> getEnv <*> getHeap

instance ( Corecursive term
         , Members (EvaluatingEffects location term value) effects
         , Recursive term
         )
      => MonadAnalysis location term value (Evaluating location term value effects) where
  type Effects location term value (Evaluating location term value effects) = EvaluatingEffects location term value

  analyzeTerm eval term = pushOrigin (termOrigin (embedSubterm term)) (eval term)

  analyzeModule eval m = pushOrigin (moduleOrigin (subterm <$> m)) (eval m)
