{-# LANGUAGE ConstraintKinds, DefaultSignatures, GADTs, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
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
, evaluatePackage
, evaluatePackageBody
, throwEvalError
, resolve
, traceResolve
, listModulesInDir
, require
, load
) where

import           Control.Abstract.Addressable as X
import           Control.Abstract.Analysis as X hiding (LoopControl(..), Return(..))
import           Control.Abstract.Analysis (LoopControl, Return(..))
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
import           Data.Term
import           Prologue

type MonadEvaluatable location term value effects m =
  ( Declarations term
  , FreeVariables term
  , Member (EvalClosure term value) effects
  , Member (LoopControl value) effects
  , Member (Reader (ModuleTable [Module term])) effects
  , Member (Resumable (Unspecialized value)) effects
  , Member (Resumable (LoadError term)) effects
  , Member (Resumable (EvalError value)) effects
  , Member (Resumable (ResolutionError value)) effects
  , Member (Resumable (AddressError location value)) effects
  , Member (Return value) effects
  , MonadAddressable location effects m
  , MonadEvaluator location term value effects m
  , MonadValue location value effects m
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
evaluateInScopedEnv :: MonadEvaluatable location term value effects m
                    => m effects value
                    -> m effects value
                    -> m effects value
evaluateInScopedEnv scopedEnvTerm term = do
  value <- scopedEnvTerm
  scopedEnv <- scopedEnvironment value
  maybe (throwEvalError (EnvironmentLookupError value)) (flip localEnv term . mergeEnvs) scopedEnv

-- | Look up and dereference the given 'Name', throwing an exception for free variables.
variable :: ( Member (Resumable (AddressError location value)) effects
            , Member (Resumable (EvalError value)) effects
            , MonadAddressable location effects m
            , MonadEvaluator location term value effects m
            )
         => Name
         -> m effects value
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


throwEvalError :: (Member (Resumable (EvalError value)) effects, MonadEvaluator location term value effects m) => EvalError value resume -> m effects resume
throwEvalError = throwResumable


data Unspecialized a b where
  Unspecialized :: { getUnspecialized :: Prelude.String } -> Unspecialized value value

instance Eq1 (Unspecialized a) where
  liftEq _ (Unspecialized a) (Unspecialized b) = a == b

deriving instance Eq (Unspecialized a b)
deriving instance Show (Unspecialized a b)
instance Show1 (Unspecialized a) where
  liftShowsPrec _ _ = showsPrec

-- | The 'Evaluatable' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class Evaluatable constr where
  eval :: ( Member (EvalModule term value) effects
          , Member Fail effects
          , Member (Reader ModuleInfo) effects
          , Member (Reader PackageInfo) effects
          , MonadEvaluatable location term value effects m
          )
       => SubtermAlgebra constr term (m effects value)
  default eval :: (MonadEvaluatable location term value effects m, Show1 constr) => SubtermAlgebra constr term (m effects value)
  eval expr = throwResumable (Unspecialized ("Eval unspecialized for " ++ liftShowsPrec (const (const id)) (const id) 0 expr ""))


-- Instances

-- | If we can evaluate any syntax which can occur in a 'Union', we can evaluate the 'Union'.
instance Apply Evaluatable fs => Evaluatable (Union fs) where
  eval = Prologue.apply (Proxy :: Proxy Evaluatable) eval

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
askModuleTable :: ( Member (Reader (ModuleTable [Module term])) effects
                  , MonadEvaluator location term value effects m
                  )
               => m effects (ModuleTable [Module term])
askModuleTable = raise ask

-- Resolve a list of module paths to a possible module table entry.
resolve :: MonadEvaluatable location term value effects m
        => [FilePath]
        -> m effects (Maybe ModulePath)
resolve names = do
  tbl <- askModuleTable
  pure $ find (`ModuleTable.member` tbl) names

traceResolve :: (Show a, Show b) => a -> b -> c -> c
traceResolve name path = trace ("resolved " <> show name <> " -> " <> show path)

listModulesInDir :: MonadEvaluatable location term value effects m
        => FilePath
        -> m effects [ModulePath]
listModulesInDir dir = ModuleTable.modulePathsInDir dir <$> askModuleTable

-- | Require/import another module by name and return it's environment and value.
--
-- Looks up the term's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: ( Member (EvalModule term value) effects
           , Member (Reader (ModuleTable [Module term])) effects
           , Member (Resumable (LoadError term)) effects
           , MonadEvaluator location term value effects m
           )
        => ModulePath
        -> m effects (Maybe (Environment location value, value))
require = requireWith evaluateModule

requireWith :: ( Member (Reader (ModuleTable [Module term])) effects
               , Member (Resumable (LoadError term)) effects
               , MonadEvaluator location term value effects m
               )
            => (Module term -> m effects value)
            -> ModulePath
            -> m effects (Maybe (Environment location value, value))
requireWith with name = getModuleTable >>= maybeM (loadWith with name) . fmap Just . ModuleTable.lookup name

-- | Load another module by name and return it's environment and value.
--
-- Always loads/evaluates.
load :: ( Member (EvalModule term value) effects
        , Member (Reader (ModuleTable [Module term])) effects
        , Member (Resumable (LoadError term)) effects
        , MonadEvaluator location term value effects m
        )
     => ModulePath
     -> m effects (Maybe (Environment location value, value))
load = loadWith evaluateModule

loadWith :: ( Member (Reader (ModuleTable [Module term])) effects
            , Member (Resumable (LoadError term)) effects
            , MonadEvaluator location term value effects m
            )
         => (Module term -> m effects value)
         -> ModulePath
         -> m effects (Maybe (Environment location value, value))
loadWith with name = askModuleTable >>= maybeM notFound . ModuleTable.lookup name >>= runMerging . foldMap (Merging . evalAndCache)
  where
    notFound = throwResumable (LoadError name)

    evalAndCache x = do
      let mPath = modulePath (moduleInfo x)
      LoadStack{..} <- getLoadStack
      if mPath `elem` unLoadStack
        then trace ("load (skip evaluating, circular load): " <> show mPath) (pure Nothing)
        else do
          modifyLoadStack (loadStackPush mPath)
          v <- trace ("load (evaluating): " <> show mPath) $ with x
          modifyLoadStack loadStackPop
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

-- | Evaluate a (root-level) term to a value using the semantics of the current analysis.
evalModule :: forall location term value effects m inner
           .  ( inner ~ (Reader ModuleInfo ': effects)
              , Evaluatable (Base term)
              , Member (EvalModule term value) effects
              , Member (EvalModule term value) inner
              , Member Fail inner
              , Member (Reader PackageInfo) inner
              , MonadAnalysis location term value inner m
              , MonadEvaluatable location term value inner m
              , Recursive term
              )
           => Module term
           -> m effects value
evalModule m
  = raiseHandler (interpose @(EvalModule term value) pure (\ (EvalModule m) yield -> lower @m (evalModule m) >>= yield))
  . withModuleInfo (moduleInfo m)
  $ analyzeModule (subtermValue . moduleBody) (fmap (Subterm <*> evalTerm) m)

evalTerm :: forall location term value effects m
         .  ( Evaluatable (Base term)
            , Member (EvalModule term value) effects
            , Member Fail effects
            , Member (Reader ModuleInfo) effects
            , Member (Reader PackageInfo) effects
            , MonadAnalysis location term value effects m
            , MonadEvaluatable location term value effects m
            , Recursive term
            )
         => term
         -> m effects value
evalTerm term = catchReturn @m @value
  (raiseHandler
    (interpose @(EvalClosure term value) pure (\ (EvalClosure term) yield -> lower @m (evalTerm term) >>= yield))
    (foldSubterms (analyzeTerm eval) term))
  (\ (Return value) -> pure value)

withModuleInfo :: Effectful m => ModuleInfo -> m (Reader ModuleInfo ': effects) a -> m effects a
withModuleInfo = raiseHandler . flip runReader

withPackageInfo :: Effectful m => PackageInfo -> m (Reader PackageInfo ': effects) a -> m effects a
withPackageInfo = raiseHandler . flip runReader

-- | Evaluate a given package.
evaluatePackage :: ( moduleEffects ~ (Reader ModuleInfo ': packageEffects)
                   , packageEffects ~ (Reader (ModuleTable [Module term]) ': Reader PackageInfo ': effects)
                   , Applicative (m packageEffects)
                   , Evaluatable (Base term)
                   , Member (EvalModule term value) moduleEffects
                   , Member (EvalModule term value) packageEffects
                   , Member Fail moduleEffects
                   , Member (Resumable (AddressError location value)) packageEffects
                   , Member (Resumable (EvalError value)) packageEffects
                   , Member (Resumable (LoadError term)) packageEffects
                   , MonadAddressable location packageEffects m
                   , MonadAnalysis location term value moduleEffects m
                   , MonadEvaluatable location term value moduleEffects m
                   , MonadEvaluator location term value packageEffects m
                   , MonadValue location value packageEffects m
                   , Recursive term
                   )
                => Package term
                -> m effects [value]
evaluatePackage p = withPackageInfo (packageInfo p) (evaluatePackageBody (packageBody p))

withUnevaluatedModules :: Effectful m => ModuleTable [Module term] -> m (Reader (ModuleTable [Module term]) ': effects) a -> m effects a
withUnevaluatedModules = raiseHandler . flip runReader

-- | Evaluate a given package body (module table and entry points).
evaluatePackageBody :: ( moduleEffects ~ (Reader ModuleInfo ': packageEffects)
                       , packageEffects ~ (Reader (ModuleTable [Module term]) ': effects)
                       , Applicative (m packageEffects)
                       , Evaluatable (Base term)
                       , Member (EvalModule term value) moduleEffects
                       , Member (EvalModule term value) packageEffects
                       , Member Fail moduleEffects
                       , Member (Reader PackageInfo) moduleEffects
                       , Member (Resumable (AddressError location value)) packageEffects
                       , Member (Resumable (EvalError value)) packageEffects
                       , Member (Resumable (LoadError term)) packageEffects
                       , MonadAddressable location packageEffects m
                       , MonadAnalysis location term value moduleEffects m
                       , MonadEvaluatable location term value moduleEffects m
                       , MonadEvaluator location term value packageEffects m
                       , MonadValue location value packageEffects m
                       , Recursive term
                       )
                    => PackageBody term
                    -> m effects [value]
evaluatePackageBody body
  = withUnevaluatedModules (packageModules body)
  . withPrelude (packagePrelude body)
  $ traverse (uncurry evaluateEntryPoint) (ModuleTable.toPairs (packageEntryPoints body))

evaluateEntryPoint :: ( inner ~ (Reader ModuleInfo ': effects)
                      , Evaluatable (Base term)
                      , Member (EvalModule term value) effects
                      , Member (Reader (ModuleTable [Module term])) effects
                      , Member (Resumable (AddressError location value)) effects
                      , Member (Resumable (EvalError value)) effects
                      , Member (Resumable (LoadError term)) effects
                      , Member (EvalModule term value) inner
                      , Member Fail inner
                      , Member (Reader PackageInfo) inner
                      , MonadAddressable location effects m
                      , MonadAnalysis location term value inner m
                      , MonadEvaluatable location term value inner m
                      , MonadEvaluator location term value effects m
                      , MonadValue location value effects m
                      , Recursive term
                      )
                   => ModulePath
                   -> Maybe Name
                   -> m effects value
evaluateEntryPoint m sym = do
  v <- maybe unit (pure . snd) <$> requireWith evalModule m
  maybe v ((`call` []) <=< variable) sym

withPrelude :: ( inner ~ (Reader ModuleInfo ': effects)
               , Evaluatable (Base term)
               , Member (EvalModule term value) effects
               , Member (EvalModule term value) inner
               , Member Fail inner
               , Member (Reader PackageInfo) inner
               , MonadAnalysis location term value inner m
               , MonadEvaluatable location term value inner m
               , MonadEvaluator location term value effects m
               , Recursive term
               )
            => Maybe (Module term)
            -> m effects a
            -> m effects a
withPrelude Nothing a = a
withPrelude (Just prelude) a = do
  preludeEnv <- evalModule prelude *> getEnv
  withDefaultEnvironment preludeEnv a
