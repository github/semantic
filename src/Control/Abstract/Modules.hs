{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Control.Abstract.Modules
( lookupModule
, resolve
, listModulesInDir
, require
, load
, Modules(..)
, runModules
, LoadError(..)
, moduleNotFound
, resumeLoadError
, runLoadError
, runLoadErrorWith
, ResolutionError(..)
, runResolutionError
, runResolutionErrorWith
, ModuleTable
) where

import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Exports
import Data.Abstract.Environment
import Data.Abstract.Exports as Exports
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Language
import Prologue

-- | Retrieve an evaluated module, if any. The outer 'Maybe' indicates whether we’ve begun loading the module or not, while the inner 'Maybe' indicates whether we’ve completed loading it or not. Thus, @Nothing@ means we’ve never tried to load it, @Just Nothing@ means we’ve started but haven’t yet finished loading it, and @Just (Just (env, value))@ indicates the result of a completed load.
lookupModule :: Member (Modules location value) effects => ModulePath -> Evaluator location value effects (Maybe (Maybe (Environment location value, value)))
lookupModule = send . Lookup

-- Resolve a list of module paths to a possible module table entry.
resolve :: Member (Modules location value) effects => [FilePath] -> Evaluator location value effects (Maybe ModulePath)
resolve = sendModules . Resolve

listModulesInDir :: Member (Modules location value) effects => FilePath -> Evaluator location value effects [ModulePath]
listModulesInDir = sendModules . List


-- | Require/import another module by name and return its environment and value.
--
-- Looks up the module's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: Member (Modules location value) effects => ModulePath -> Evaluator location value effects (Maybe (Environment location value, value))
require path = lookupModule path >>= maybeM (load path)

-- | Load another module by name and return its environment and value.
--
-- Always loads/evaluates.
load :: Member (Modules location value) effects => ModulePath -> Evaluator location value effects (Maybe (Environment location value, value))
load = send . Load


data Modules location value return where
  Load    :: ModulePath -> Modules location value (Maybe (Environment location value, value))
  Lookup  :: ModulePath -> Modules location value (Maybe (Maybe (Environment location value, value)))
  Resolve :: [FilePath] -> Modules location value (Maybe ModulePath)
  List    :: FilePath   -> Modules location value [ModulePath]

sendModules :: Member (Modules location value) effects => Modules location value return -> Evaluator location value effects return
sendModules = send

runModules :: forall term location value effects a
           .  Members '[ Resumable (LoadError location value)
                       , State (Environment location value)
                       , State (Exports location value)
                       , State (ModuleTable (Maybe (Environment location value, value)))
                       , Trace
                       ] effects
           => (Module term -> Evaluator location value (Modules location value ': effects) value)
           -> Evaluator location value (Modules location value ': effects) a
           -> Evaluator location value (Reader (ModuleTable [Module term]) ': effects) a
runModules evaluateModule = go
  where go :: forall a . Evaluator location value (Modules location value ': effects) a -> Evaluator location value (Reader (ModuleTable [Module term]) ': effects) a
        go = reinterpretEffect (\ m -> case m of
          Load name -> askModuleTable @term >>= maybe (moduleNotFound name) (runMerging . foldMap (Merging . evalAndCache)) . ModuleTable.lookup name
            where
              evalAndCache x = do
                let mPath = modulePath (moduleInfo x)
                loading <- loadingModule mPath
                if loading
                  then traceE ("load (skip evaluating, circular load): " <> show mPath) $> Nothing
                  else do
                    cacheModule name Nothing
                    v <- traceE ("load (evaluating): " <> show mPath) *> go (evaluateModule x) <* traceE ("load done:" <> show mPath)
                    env <- filterEnv <$> getExports <*> getEnv
                    cacheModule name (Just (env, v))
                    pure (Just (env, v))

              -- TODO: If the set of exports is empty because no exports have been
              -- defined, do we export all terms, or no terms? This behavior varies across
              -- languages. We need better semantics rather than doing it ad-hoc.
              filterEnv :: Exports.Exports location value -> Environment location value -> Environment location value
              filterEnv ports env
                | Exports.null ports = env
                | otherwise = Exports.toEnvironment ports `mergeEnvs` overwrite (Exports.aliases ports) env
          Lookup path -> ModuleTable.lookup path <$> raise get
          Resolve names -> do
            isMember <- flip ModuleTable.member <$> askModuleTable @term
            pure (find isMember names)
          List dir -> modulePathsInDir dir <$> askModuleTable @term)


loadingModule :: Member (State (ModuleTable (Maybe (Environment location value, value)))) effects => ModulePath -> Evaluator location value effects Bool
loadingModule path = isJust . ModuleTable.lookup path <$> getModuleTable

getModuleTable :: Member (State (ModuleTable (Maybe (Environment location value, value)))) effects => Evaluator location value effects (ModuleTable (Maybe (Environment location value, value)))
getModuleTable = raise get

cacheModule :: Member (State (ModuleTable (Maybe (Environment location value, value)))) effects => ModulePath -> Maybe (Environment location value, value) -> Evaluator location value effects ()
cacheModule path result = raise (modify' (ModuleTable.insert path result))

askModuleTable :: Member (Reader (ModuleTable [Module term])) effects => Evaluator location value effects (ModuleTable [Module term])
askModuleTable = raise ask


newtype Merging m location value = Merging { runMerging :: m (Maybe (Environment location value, value)) }

instance Applicative m => Semigroup (Merging m location value) where
  Merging a <> Merging b = Merging (merge <$> a <*> b)
    where merge a b = mergeJusts <$> a <*> b <|> a <|> b
          mergeJusts (env1, _) (env2, v) = (mergeEnvs env1 env2, v)

instance Applicative m => Monoid (Merging m location value) where
  mappend = (<>)
  mempty = Merging (pure Nothing)


-- | An error thrown when loading a module from the list of provided modules. Indicates we weren't able to find a module with the given name.
data LoadError location value resume where
  ModuleNotFound :: ModulePath -> LoadError location value (Maybe (Environment location value, value))

deriving instance Eq (LoadError location value resume)
deriving instance Show (LoadError location value resume)
instance Show1 (LoadError location value) where
  liftShowsPrec _ _ = showsPrec
instance Eq1 (LoadError location value) where
  liftEq _ (ModuleNotFound a) (ModuleNotFound b) = a == b

moduleNotFound :: Member (Resumable (LoadError location value)) effects => ModulePath -> Evaluator location value effects (Maybe (Environment location value, value))
moduleNotFound = throwResumable . ModuleNotFound

resumeLoadError :: Member (Resumable (LoadError location value)) effects => Evaluator location value effects a -> (forall resume . LoadError location value resume -> Evaluator location value effects resume) -> Evaluator location value effects a
resumeLoadError = resume

runLoadError :: Evaluator location value (Resumable (LoadError location value) ': effects) a -> Evaluator location value effects (Either (SomeExc (LoadError location value)) a)
runLoadError = raiseHandler runError

runLoadErrorWith :: (forall resume . LoadError location value resume -> Evaluator location value effects resume) -> Evaluator location value (Resumable (LoadError location value) ': effects) a -> Evaluator location value effects a
runLoadErrorWith = runResumableWith


-- | An error thrown when we can't resolve a module from a qualified name.
data ResolutionError resume where
  NotFoundError :: String   -- ^ The path that was not found.
                -> [String] -- ^ List of paths searched that shows where semantic looked for this module.
                -> Language -- ^ Language.
                -> ResolutionError ModulePath

  GoImportError :: FilePath -> ResolutionError [ModulePath]

deriving instance Eq (ResolutionError b)
deriving instance Show (ResolutionError b)
instance Show1 ResolutionError where liftShowsPrec _ _ = showsPrec
instance Eq1 ResolutionError where
  liftEq _ (NotFoundError a _ l1) (NotFoundError b _ l2) = a == b && l1 == l2
  liftEq _ (GoImportError a) (GoImportError b) = a == b
  liftEq _ _ _ = False

runResolutionError :: Effectful m => m (Resumable ResolutionError ': effects) a -> m effects (Either (SomeExc ResolutionError) a)
runResolutionError = raiseHandler runError

runResolutionErrorWith :: Effectful m => (forall resume . ResolutionError resume -> m effects resume) -> m (Resumable ResolutionError ': effects) a -> m effects a
runResolutionErrorWith = runResumableWith
