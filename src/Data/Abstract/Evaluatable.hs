{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Evaluatable
( module X
, Evaluatable(..)
, evaluatePackageWith
, traceResolve
-- | Effects
, EvalError(..)
, throwEvalError
, runEvalError
, runEvalErrorWith
, Unspecialized(..)
, runUnspecialized
, runUnspecializedWith
, Cell
) where

import Algebra.Graph.Class (foldg)
import Control.Abstract hiding (Load)
import Control.Abstract.Context as X
import Control.Abstract.Environment as X hiding (runEnvironmentError, runEnvironmentErrorWith)
import Control.Abstract.Evaluator as X hiding (LoopControl(..), Return(..), catchLoopControl, runLoopControl, catchReturn, runReturn)
import Control.Abstract.Heap as X hiding (AddressError(..), runAddressError, runAddressErrorWith)
import Control.Abstract.Modules as X (Modules, ResolutionError(..), load, lookupModule, listModulesInDir, require, resolve)
import Control.Abstract.Value as X
import Data.Abstract.Declarations as X
import Data.Abstract.Environment as X
import Data.Abstract.FreeVariables as X
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Abstract.Name as X
import Data.Abstract.Package as Package
import Data.Abstract.Ref as X
import Data.Graph
import Data.List (groupBy, nub, sortBy)
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import qualified Data.Monoid as Monoid
import qualified Data.Map.Monoidal as Map
import Data.Ord (comparing)
import Data.Scientific (Scientific)
import Data.Semigroup.App
import Data.Semigroup.Foldable
import Data.Semigroup.Reducer hiding (unit)
import Data.Sum
import Data.Term
import Prologue

-- | The 'Evaluatable' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class Show1 constr => Evaluatable constr where
  eval :: ( AbstractValue address value effects
          , Declarations term
          , FreeVariables term
          , Member (Allocator address value) effects
          , Member (Env address) effects
          , Member (LoopControl address) effects
          , Member (Modules address value) effects
          , Member (Reader ModuleInfo) effects
          , Member (Reader PackageInfo) effects
          , Member (Reader Span) effects
          , Member (Resumable (EnvironmentError address)) effects
          , Member (Resumable EvalError) effects
          , Member (Resumable ResolutionError) effects
          , Member (Resumable (Unspecialized value)) effects
          , Member (Return address) effects
          , Member Trace effects
          )
       => SubtermAlgebra constr term (Evaluator address value effects (ValueRef address))
  eval expr = rvalBox =<< throwResumable (Unspecialized ("Eval unspecialized for " ++ liftShowsPrec (const (const id)) (const id) 0 expr ""))


topologicalSort :: Ord v => Graph v -> [NonEmpty v]
topologicalSort
  = groupByInEdgeCount
  . allVertices
  . labelWithInEdgeCounts

labelWithInEdgeCounts :: Ord v => Graph v -> Graph (Monoid.Sum Int, v)
labelWithInEdgeCounts
  = uncurry mapGraph
  . foldg
    (lowerBound, lowerBound)
    ((,) lowerBound . vertex)
    (<>)
    (\ (outM, outG) (inM, inG) ->
      ( outM <> inM <> foldMap (flip Map.singleton (Monoid.Sum (length outG))) (allVertices inG)
      , outG `connect` inG
      ))
  where mapGraph edgeCountsByVertex g = pairWithCountIn edgeCountsByVertex <$> g
        pairWithCountIn edgeCountsByVertex vertex = (fromMaybe 0 (Map.lookup vertex edgeCountsByVertex), vertex)

allVertices :: Eq v => Graph v -> [v]
allVertices = nub . toList

groupByInEdgeCount :: Ord sum => [(sum, v)] -> [NonEmpty v]
groupByInEdgeCount = map (NonEmpty.fromList . map snd) . groupBy ((==) `on` fst) . sortBy (comparing fst)


evaluate :: forall address term value effects
         .  ( AbstractValue address value (LoopControl address ': Return address ': Env address ': Allocator address value ': Reader ModuleInfo ': Modules address value ': effects)
            , Addressable address (Reader ModuleInfo ': Modules address value ': effects)
            , Declarations term
            , Evaluatable (Base term)
            , Foldable (Cell address)
            , FreeVariables term
            , Member (Reader (ModuleTable (NonEmpty (Module (address, Environment address))))) effects
            , Member (Reader PackageInfo) effects
            , Member (Reader Span) effects
            , Member (Resumable (AddressError address value)) effects
            , Member (Resumable (EnvironmentError address)) effects
            , Member (Resumable EvalError) effects
            , Member (Resumable (LoadError address value)) effects
            , Member (Resumable ResolutionError) effects
            , Member (Resumable (Unspecialized value)) effects
            , Member (State (Heap address (Cell address) value)) effects
            , Member (State (ModuleTable (Maybe (address, Environment address)))) effects
            , Member Trace effects
            , Recursive term
            , Reducer value (Cell address value)
            , ValueRoots address value
            )
         => [NonEmpty (Module term)]
         -> Evaluator address value effects (ModuleTable (NonEmpty (Module (address, Environment address))))
evaluate [] = ask
evaluate (modules : rest)
  = runRestOfLoadOrder
  . runReader lowerBound
  . runModules evalModule
  $ traverse evalModuleAndRetain modules
  where evalModule :: Module term -> Evaluator address value (Modules address value ': effects) (address, Environment address)
        evalModule m
          = runReader (moduleInfo m)
          . runAllocator
          . runEnv lowerBound
          . runReturn
          . runLoopControl
          $ foldSubterms eval (moduleBody m) >>= address

        evalModuleAndRetain m = (<$ m) <$> evalModule m

        runRestOfLoadOrder action = do
          results <- action
          local (<> ModuleTable.fromModules (toList results)) (evaluate rest)

-- | Evaluate a given package.
evaluatePackageWith :: forall address term value inner inner' inner'' outer
                    .  ( AbstractValue address value inner
                       -- FIXME: It’d be nice if we didn’t have to mention 'Addressable' here at all, but 'Located' locations require knowledge of 'currentModule' to run. Can we fix that?
                       , Addressable address inner'
                       , Declarations term
                       , Evaluatable (Base term)
                       , Foldable (Cell address)
                       , FreeVariables term
                       , Member Fresh outer
                       , Member (Resumable (AddressError address value)) outer
                       , Member (Resumable (EnvironmentError address)) outer
                       , Member (Resumable EvalError) outer
                       , Member (Resumable (LoadError address value)) outer
                       , Member (Resumable ResolutionError) outer
                       , Member (Resumable (Unspecialized value)) outer
                       , Member (State (Heap address (Cell address) value)) outer
                       , Member (State (ModuleTable (Maybe (address, Environment address)))) outer
                       , Member Trace outer
                       , Recursive term
                       , Reducer value (Cell address value)
                       , ValueRoots address value
                       , inner ~ (LoopControl address ': Return address ': Env address ': Allocator address value ': inner')
                       , inner' ~ (Reader ModuleInfo ': inner'')
                       , inner'' ~ (Modules address value ': Reader Span ': Reader PackageInfo ': outer)
                       )
                    => (SubtermAlgebra Module      term (TermEvaluator term address value inner address)                  -> SubtermAlgebra Module      term (TermEvaluator term address value inner address))
                    -> (SubtermAlgebra (Base term) term (TermEvaluator term address value inner (ValueRef address)) -> SubtermAlgebra (Base term) term (TermEvaluator term address value inner (ValueRef address)))
                    -> Package term
                    -> TermEvaluator term address value outer [(address, Environment address)]
evaluatePackageWith analyzeModule analyzeTerm package
  = runReader (packageInfo package)
  . runReader lowerBound
  . runReader (packageModules (packageBody package))
  . withPrelude (packagePrelude (packageBody package))
  $ \ preludeEnv
  ->  raiseHandler (runModules (runTermEvaluator . evalModule preludeEnv))
    . traverse (uncurry (evaluateEntryPoint preludeEnv))
    $ ModuleTable.toPairs (packageEntryPoints (packageBody package))
  where
        evalModule preludeEnv m
          = runInModule preludeEnv (moduleInfo m)
          . analyzeModule (subtermRef . moduleBody)
          $ evalTerm <$> m
        evalTerm term = Subterm term (TermEvaluator (address =<< runTermEvaluator (foldSubterms (analyzeTerm (TermEvaluator . eval . fmap (second runTermEvaluator))) term)))

        runInModule preludeEnv info
          = runReader info
          . raiseHandler runAllocator
          . raiseHandler (runEnv preludeEnv)
          . raiseHandler runReturn
          . raiseHandler runLoopControl

        evaluateEntryPoint :: Environment address -> ModulePath -> Maybe Name -> TermEvaluator term address value inner'' (address, Environment address)
        evaluateEntryPoint preludeEnv m sym = runInModule preludeEnv (ModuleInfo m) . TermEvaluator $ do
          addr <- box unit -- TODO don't *always* allocate - use maybeM instead
          (ptr, env) <- fromMaybe (addr, lowerBound) <$> require m
          bindAll env
          maybe (pure ptr) ((`call` []) <=< deref <=< variable) sym

        evalPrelude prelude = raiseHandler (runModules (runTermEvaluator . evalModule lowerBound)) $ do
          (_, builtinsEnv) <- runInModule lowerBound moduleInfoFromCallStack (TermEvaluator (defineBuiltins *> box unit))
          second (mergeEnvs builtinsEnv) <$> evalModule builtinsEnv prelude

        withPrelude Nothing f = f lowerBound
        withPrelude (Just prelude) f = do
          (_, preludeEnv) <- evalPrelude prelude
          f preludeEnv


traceResolve :: (Show a, Show b, Member Trace effects) => a -> b -> Evaluator address value effects ()
traceResolve name path = trace ("resolved " <> show name <> " -> " <> show path)


-- Effects

-- | The type of error thrown when failing to evaluate a term.
data EvalError return where
  FreeVariablesError :: [Name] -> EvalError Name
  -- Indicates that our evaluator wasn't able to make sense of these literals.
  IntegerFormatError  :: Text -> EvalError Integer
  FloatFormatError    :: Text -> EvalError Scientific
  RationalFormatError :: Text -> EvalError Rational
  DefaultExportError  :: EvalError ()
  ExportError         :: ModulePath -> Name -> EvalError ()

deriving instance Eq (EvalError return)
deriving instance Show (EvalError return)

instance Eq1 EvalError where
  liftEq _ (FreeVariablesError a) (FreeVariablesError b)   = a == b
  liftEq _ DefaultExportError DefaultExportError           = True
  liftEq _ (ExportError a b) (ExportError c d)             = (a == c) && (b == d)
  liftEq _ (IntegerFormatError a) (IntegerFormatError b)   = a == b
  liftEq _ (FloatFormatError a) (FloatFormatError b)       = a == b
  liftEq _ (RationalFormatError a) (RationalFormatError b) = a == b
  liftEq _ _ _                                             = False

instance Show1 EvalError where
  liftShowsPrec _ _ = showsPrec

throwEvalError :: (Effectful m, Member (Resumable EvalError) effects) => EvalError resume -> m effects resume
throwEvalError = throwResumable

runEvalError :: Effectful m => m (Resumable EvalError ': effects) a -> m effects (Either (SomeExc EvalError) a)
runEvalError = runResumable

runEvalErrorWith :: Effectful m => (forall resume . EvalError resume -> m effects resume) -> m (Resumable EvalError ': effects) a -> m effects a
runEvalErrorWith = runResumableWith


data Unspecialized a b where
  Unspecialized :: String -> Unspecialized value value

deriving instance Eq (Unspecialized a b)
deriving instance Show (Unspecialized a b)

instance Eq1 (Unspecialized a) where
  liftEq _ (Unspecialized a) (Unspecialized b) = a == b

instance Show1 (Unspecialized a) where
  liftShowsPrec _ _ = showsPrec

runUnspecialized :: Effectful (m value) => m value (Resumable (Unspecialized value) ': effects) a -> m value effects (Either (SomeExc (Unspecialized value)) a)
runUnspecialized = runResumable

runUnspecializedWith :: Effectful (m value) => (forall resume . Unspecialized value resume -> m value effects resume) -> m value (Resumable (Unspecialized value) ': effects) a -> m value effects a
runUnspecializedWith = runResumableWith


-- Instances

-- | If we can evaluate any syntax which can occur in a 'Sum', we can evaluate the 'Sum'.
instance (Apply Evaluatable fs, Apply Show1 fs) => Evaluatable (Sum fs) where
  eval = apply @Evaluatable eval

-- | Evaluating a 'TermF' ignores its annotation, evaluating the underlying syntax.
instance (Evaluatable s, Show a) => Evaluatable (TermF s a) where
  eval = eval . termFOut


-- NOTE: Use 'Data.Syntax.Statements' instead of '[]' if you need imperative eval semantics.
--
-- | '[]' is treated as an imperative sequence of statements/declarations s.t.:
--
--   1. Each statement’s effects on the store are accumulated;
--   2. Each statement can affect the environment of later statements (e.g. by 'modify'-ing the environment); and
--   3. Only the last statement’s return value is returned.
instance Evaluatable [] where
  -- 'nonEmpty' and 'foldMap1' enable us to return the last statement’s result instead of 'unit' for non-empty lists.
  eval = maybe (rvalBox unit) (runApp . foldMap1 (App . subtermRef)) . nonEmpty
