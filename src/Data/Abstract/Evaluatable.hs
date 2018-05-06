{-# LANGUAGE ConstraintKinds, DefaultSignatures, GADTs, RankNTypes, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Evaluatable
( module X
, MonadEvaluatable
, Evaluatable(..)
, Unspecialized(..)
, EvalError(..)
, LoadError(..)
, ResolutionError(..)
, variable
, evaluateInScopedEnv
, evaluatePackageWith
, evaluatePackageBodyWith
, throwEvalError
, resolve
, traceResolve
, listModulesInDir
, require
, load
) where

import           Control.Abstract.Addressable as X
import           Control.Abstract.Evaluator as X hiding (LoopControl(..), Return(..))
import           Control.Abstract.Evaluator (LoopControl, Return(..))
import           Control.Abstract.Value as X
import           Control.Monad.Effect as Eff
import           Data.Abstract.Address
import           Data.Abstract.Declarations as X
import           Data.Abstract.Environment as X
import qualified Data.Abstract.Exports as Exports
import           Data.Abstract.FreeVariables as X
import           Data.Abstract.Module
import           Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Package as Package
import           Data.Language
import           Data.Scientific (Scientific)
import           Data.Semigroup.App
import           Data.Semigroup.Foldable
import           Data.Semigroup.Reducer hiding (unit)
import           Data.Sum
import           Data.Term
import           Prologue

-- | The 'Evaluatable' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class Evaluatable constr where
  eval :: ( Member Fail effects
          , MonadEvaluatable location term value effects
          )
       => SubtermAlgebra constr term (Evaluator location term value effects value)
  default eval :: (Member (Resumable (Unspecialized value)) effects, Show1 constr) => SubtermAlgebra constr term (Evaluator location term value effects value)
  eval expr = throwResumable (Unspecialized ("Eval unspecialized for " ++ liftShowsPrec (const (const id)) (const id) 0 expr ""))

type MonadEvaluatable location term value effects =
  ( AbstractValue location term value effects
  , Addressable location effects
  , Declarations term
  , FreeVariables term
  , Members '[ EvalClosure term value
             , EvalModule term value
             , LoopControl value
             , Reader (Environment location value)
             , Reader LoadStack
             , Reader ModuleInfo
             , Reader (ModuleTable [Module term])
             , Reader PackageInfo
             , Resumable (AddressError location value)
             , Resumable (EvalError value)
             , Resumable (LoadError term)
             , Resumable (ResolutionError value)
             , Resumable (Unspecialized value)
             , Return value
             , State (Environment location value)
             , State (Exports location value)
             , State (Heap location value)
             , State (ModuleTable (Environment location value, value))
             ] effects
  , Reducer value (Cell location value)
  )

-- | An error thrown when we can't resolve a module from a qualified name.
data ResolutionError value resume where
  NotFoundError :: String   -- ^ The path that was not found.
                -> [String] -- ^ List of paths searched that shows where semantic looked for this module.
                -> Language -- ^ Language.
                -> ResolutionError value ModulePath

  GoImportError :: FilePath -> ResolutionError value [ModulePath]

deriving instance Eq (ResolutionError a b)
deriving instance Show (ResolutionError a b)
instance Show1 (ResolutionError value) where liftShowsPrec _ _ = showsPrec
instance Eq1 (ResolutionError value) where
  liftEq _ (NotFoundError a _ l1) (NotFoundError b _ l2) = a == b && l1 == l2
  liftEq _ (GoImportError a) (GoImportError b) = a == b
  liftEq _ _ _ = False

-- | An error thrown when loading a module from the list of provided modules. Indicates we weren't able to find a module with the given name.
data LoadError term resume where
  LoadError :: ModulePath -> LoadError term [Module term]

deriving instance Eq (LoadError term resume)
deriving instance Show (LoadError term resume)
instance Show1 (LoadError term) where
  liftShowsPrec _ _ = showsPrec
instance Eq1 (LoadError term) where
  liftEq _ (LoadError a) (LoadError b) = a == b

-- | The type of error thrown when failing to evaluate a term.
data EvalError value resume where
  -- Indicates we weren't able to dereference a name from the evaluated environment.
  FreeVariableError :: Name -> EvalError value value
  FreeVariablesError :: [Name] -> EvalError value Name
  -- Indicates that our evaluator wasn't able to make sense of these literals.
  IntegerFormatError  :: ByteString -> EvalError value Integer
  FloatFormatError    :: ByteString -> EvalError value Scientific
  RationalFormatError :: ByteString -> EvalError value Rational
  DefaultExportError  :: EvalError value ()
  ExportError         :: ModulePath -> Name -> EvalError value ()
  EnvironmentLookupError :: value -> EvalError value value

-- | Evaluate a term within the context of the scoped environment of 'scopedEnvTerm'.
--   Throws an 'EnvironmentLookupError' if @scopedEnvTerm@ does not have an environment.
evaluateInScopedEnv :: ( AbstractValue location term value effects
                       , Members '[ Resumable (EvalError value)
                                  , State (Environment location value)
                                  ] effects
                       )
                    => Evaluator location term value effects value
                    -> Evaluator location term value effects value
                    -> Evaluator location term value effects value
evaluateInScopedEnv scopedEnvTerm term = do
  value <- scopedEnvTerm
  scopedEnv <- scopedEnvironment value
  maybe (throwEvalError (EnvironmentLookupError value)) (flip localEnv term . mergeEnvs) scopedEnv

-- | Look up and dereference the given 'Name', throwing an exception for free variables.
variable :: ( Addressable location effects
            , Members '[ Reader (Environment location value)
                       , Resumable (AddressError location value)
                       , Resumable (EvalError value)
                       , State (Environment location value)
                       , State (Heap location value)
                       ] effects
            )
         => Name
         -> Evaluator location term value effects value
variable name = lookupWith deref name >>= maybeM (throwResumable (FreeVariableError name))

deriving instance Eq a => Eq (EvalError a b)
deriving instance Show a => Show (EvalError a b)
instance Show value => Show1 (EvalError value) where
  liftShowsPrec _ _ = showsPrec
instance Eq term => Eq1 (EvalError term) where
  liftEq _ (FreeVariableError a) (FreeVariableError b)     = a == b
  liftEq _ (FreeVariablesError a) (FreeVariablesError b)   = a == b
  liftEq _ DefaultExportError DefaultExportError           = True
  liftEq _ (ExportError a b) (ExportError c d)             = (a == c) && (b == d)
  liftEq _ (IntegerFormatError a) (IntegerFormatError b)   = a == b
  liftEq _ (FloatFormatError a) (FloatFormatError b)       = a == b
  liftEq _ (RationalFormatError a) (RationalFormatError b) = a == b
  liftEq _ (EnvironmentLookupError a) (EnvironmentLookupError b) = a == b
  liftEq _ _ _                                             = False


throwEvalError :: Member (Resumable (EvalError value)) effects => EvalError value resume -> Evaluator location term value effects resume
throwEvalError = throwResumable


data Unspecialized a b where
  Unspecialized :: { getUnspecialized :: Prelude.String } -> Unspecialized value value

instance Eq1 (Unspecialized a) where
  liftEq _ (Unspecialized a) (Unspecialized b) = a == b

deriving instance Eq (Unspecialized a b)
deriving instance Show (Unspecialized a b)
instance Show1 (Unspecialized a) where
  liftShowsPrec _ _ = showsPrec


-- Instances

-- | If we can evaluate any syntax which can occur in a 'Sum', we can evaluate the 'Sum'.
instance Apply Evaluatable fs => Evaluatable (Sum fs) where
  eval = apply @Evaluatable eval

-- | Evaluating a 'TermF' ignores its annotation, evaluating the underlying syntax.
instance Evaluatable s => Evaluatable (TermF s a) where
  eval = eval . termFOut

--- | '[]' is treated as an imperative sequence of statements/declarations s.t.:
---
---   1. Each statement’s effects on the store are accumulated;
---   2. Each statement can affect the environment of later statements (e.g. by 'modify'-ing the environment); and
---   3. Only the last statement’s return value is returned.
instance Evaluatable [] where
  -- 'nonEmpty' and 'foldMap1' enable us to return the last statement’s result instead of 'unit' for non-empty lists.
  eval = maybe unit (runApp . foldMap1 (App . subtermValue)) . nonEmpty

-- | Retrieve the table of unevaluated modules.
askModuleTable :: Member (Reader (ModuleTable [Module term])) effects
               => Evaluator location term value effects (ModuleTable [Module term])
askModuleTable = raise ask

-- Resolve a list of module paths to a possible module table entry.
resolve :: Member (Reader (ModuleTable [Module term])) effects
        => [FilePath]
        -> Evaluator location term value effects (Maybe ModulePath)
resolve names = do
  tbl <- askModuleTable
  pure $ find (`ModuleTable.member` tbl) names

traceResolve :: (Show a, Show b) => a -> b -> c -> c
traceResolve name path = trace ("resolved " <> show name <> " -> " <> show path)

listModulesInDir :: Member (Reader (ModuleTable [Module term])) effects
                 => FilePath
                 -> Evaluator location term value effects [ModulePath]
listModulesInDir dir = ModuleTable.modulePathsInDir dir <$> askModuleTable

-- | Require/import another module by name and return it's environment and value.
--
-- Looks up the term's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: Members '[ EvalModule term value
                    , Reader (ModuleTable [Module term])
                    , Reader LoadStack
                    , Resumable (LoadError term)
                    , State (Environment location value)
                    , State (Exports location value)
                    , State (ModuleTable (Environment location value, value))
                    ] effects
        => ModulePath
        -> Evaluator location term value effects (Maybe (Environment location value, value))
require = requireWith evaluateModule

requireWith :: Members '[ Reader (ModuleTable [Module term])
                        , Reader LoadStack
                        , Resumable (LoadError term)
                        , State (Environment location value)
                        , State (Exports location value)
                        , State (ModuleTable (Environment location value, value))
                        ] effects
            => (Module term -> Evaluator location term value effects value)
            -> ModulePath
            -> Evaluator location term value effects (Maybe (Environment location value, value))
requireWith with name = getModuleTable >>= maybeM (loadWith with name) . fmap Just . ModuleTable.lookup name

-- | Load another module by name and return it's environment and value.
--
-- Always loads/evaluates.
load :: Members '[ EvalModule term value
                 , Reader (ModuleTable [Module term])
                 , Reader LoadStack
                 , Resumable (LoadError term)
                 , State (Environment location value)
                 , State (Exports location value)
                 , State (ModuleTable (Environment location value, value))
                 ] effects
     => ModulePath
     -> Evaluator location term value effects (Maybe (Environment location value, value))
load = loadWith evaluateModule

loadWith :: Members '[ Reader (ModuleTable [Module term])
                     , Reader LoadStack
                     , Resumable (LoadError term)
                     , State (Environment location value)
                     , State (Exports location value)
                     , State (ModuleTable (Environment location value, value))
                     ] effects
         => (Module term -> Evaluator location term value effects value)
         -> ModulePath
         -> Evaluator location term value effects (Maybe (Environment location value, value))
loadWith with name = askModuleTable >>= maybeM notFound . ModuleTable.lookup name >>= runMerging . foldMap (Merging . evalAndCache)
  where
    notFound = throwResumable (LoadError name)

    evalAndCache x = do
      let mPath = modulePath (moduleInfo x)
      LoadStack{..} <- askLoadStack
      if moduleInfo x `elem` unLoadStack
        then trace ("load (skip evaluating, circular load): " <> show mPath) (pure Nothing)
        else do
          v <- trace ("load (evaluating): " <> show mPath) (with x)
          traceM ("load done:" <> show mPath)
          env <- filterEnv <$> getExports <*> getEnv
          modifyModuleTable (ModuleTable.insert name (env, v))
          pure (Just (env, v))

    -- TODO: If the set of exports is empty because no exports have been
    -- defined, do we export all terms, or no terms? This behavior varies across
    -- languages. We need better semantics rather than doing it ad-hoc.
    filterEnv :: Exports.Exports l a -> Environment l a -> Environment l a
    filterEnv ports env
      | Exports.null ports = env
      | otherwise = Exports.toEnvironment ports `mergeEnvs` overwrite (Exports.aliases ports) env

newtype Merging m location value = Merging { runMerging :: m (Maybe (Environment location value, value)) }

instance Applicative m => Semigroup (Merging m location value) where
  Merging a <> Merging b = Merging (merge <$> a <*> b)
    where merge a b = mergeJusts <$> a <*> b <|> a <|> b
          mergeJusts (env1, _) (env2, v) = (mergeEnvs env1 env2, v)

instance Applicative m => Monoid (Merging m location value) where
  mappend = (<>)
  mempty = Merging (pure Nothing)


-- | Evaluate a given package.
evaluatePackageWith :: ( AbstractValue location term value moduleEffects
                       , Addressable location moduleEffects
                       , Evaluatable (Base term)
                       , Member Fail effects
                       , Members '[ Reader (Environment location value)
                                  , Reader LoadStack
                                  , Resumable (AddressError location value)
                                  , Resumable (EvalError value)
                                  , Resumable (LoadError term)
                                  , State (Environment location value)
                                  , State (Exports location value)
                                  , State (Heap location value)
                                  , State (ModuleTable (Environment location value, value))
                                  ] effects
                       , MonadEvaluatable location term value termEffects
                       , Recursive term
                       , termEffects ~ (EvalClosure term value ': moduleEffects)
                       , moduleEffects ~ (EvalModule term value ': packageBodyEffects)
                       , packageBodyEffects ~ (Reader (ModuleTable [Module term]) ': packageEffects)
                       , packageEffects ~ (Reader PackageInfo ': effects)
                       )
                    => (SubtermAlgebra Module term (Evaluator location term value moduleEffects value) -> SubtermAlgebra Module term (Evaluator location term value moduleEffects value))
                    -> (SubtermAlgebra (Base term) term (Evaluator location term value termEffects value) -> SubtermAlgebra (Base term) term (Evaluator location term value termEffects value))
                    -> Package term
                    -> Evaluator location term value effects [value]
evaluatePackageWith perModule perTerm = handleReader . packageInfo <*> evaluatePackageBodyWith perModule perTerm . packageBody

-- | Evaluate a given package body (module table and entry points).
evaluatePackageBodyWith :: forall location term value effects termEffects moduleEffects packageBodyEffects
                        .  ( AbstractValue location term value moduleEffects
                           , Addressable location moduleEffects
                           , Evaluatable (Base term)
                           , Member Fail effects
                           , Members '[ Reader (Environment location value)
                                      , Reader LoadStack
                                      , Resumable (AddressError location value)
                                      , Resumable (EvalError value)
                                      , Resumable (LoadError term)
                                      , State (Environment location value)
                                      , State (Exports location value)
                                      , State (Heap location value)
                                      , State (ModuleTable (Environment location value, value))
                                      ] effects
                           , MonadEvaluatable location term value termEffects
                           , Recursive term
                           , termEffects ~ (EvalClosure term value ': moduleEffects)
                           , moduleEffects ~ (EvalModule term value ': packageBodyEffects)
                           , packageBodyEffects ~ (Reader (ModuleTable [Module term]) ': effects)
                           )
                        => (SubtermAlgebra Module term (Evaluator location term value moduleEffects value) -> SubtermAlgebra Module term (Evaluator location term value moduleEffects value))
                        -> (SubtermAlgebra (Base term) term (Evaluator location term value termEffects value) -> SubtermAlgebra (Base term) term (Evaluator location term value termEffects value))
                        -> PackageBody term
                        -> Evaluator location term value effects [value]
evaluatePackageBodyWith perModule perTerm body
  = handleReader (packageModules body)
  . handleEvalModules
  . withPrelude (packagePrelude body)
  $ traverse (uncurry evaluateEntryPoint) (ModuleTable.toPairs (packageEntryPoints body))
  where handleEvalModules :: Evaluator location term value moduleEffects a -> Evaluator location term value packageBodyEffects a
        handleEvalModules = raiseHandler (relay pure (\ (EvalModule m) yield -> lower (evalModule m) >>= yield))
        evalModule
          = handleEvalModules
          . perModule (subtermValue . moduleBody)
          . fmap (Subterm <*> evalTerm)
        evalTerm
          = raiseHandler (relay pure (\ (EvalClosure term) yield -> lower (evalTerm term) >>= yield))
          . foldSubterms (perTerm eval)

evaluateEntryPoint :: ( AbstractValue location term value effects
                      , Addressable location effects
                      , Members '[ EvalModule term value
                                 , Reader (Environment location value)
                                 , Reader LoadStack
                                 , Reader (ModuleTable [Module term])
                                 , Resumable (AddressError location value)
                                 , Resumable (EvalError value)
                                 , Resumable (LoadError term)
                                 , State (Environment location value)
                                 , State (Exports location value)
                                 , State (Heap location value)
                                 , State (ModuleTable (Environment location value, value))
                                 ] effects
                      )
                   => ModulePath
                   -> Maybe Name
                   -> Evaluator location term value effects value
evaluateEntryPoint m sym = do
  v <- maybe unit (pure . snd) <$> require m
  maybe v ((`call` []) <=< variable) sym

withPrelude :: Members '[ EvalModule term value
                        , Reader (Environment location value)
                        , State (Environment location value)
                        ] effects
            => Maybe (Module term)
            -> Evaluator location term value effects a
            -> Evaluator location term value effects a
withPrelude Nothing a = a
withPrelude (Just prelude) a = do
  preludeEnv <- evaluateModule prelude *> getEnv
  withDefaultEnvironment preludeEnv a
