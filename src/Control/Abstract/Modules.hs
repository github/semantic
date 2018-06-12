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

import Control.Abstract.Evaluator
import Data.Abstract.Environment
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Language
import Prologue

-- | Retrieve an evaluated module, if any. The outer 'Maybe' indicates whether we’ve begun loading the module or not, while the inner 'Maybe' indicates whether we’ve completed loading it or not. Thus, @Nothing@ means we’ve never tried to load it, @Just Nothing@ means we’ve started but haven’t yet finished loading it, and @Just (Just (env, value))@ indicates the result of a completed load.
lookupModule :: Member (Modules address value) effects => ModulePath -> Evaluator address value effects (Maybe (Maybe (address, Environment address)))
lookupModule = sendModules . Lookup

-- | Resolve a list of module paths to a possible module table entry.
resolve :: forall address value effects . Member (Modules address value) effects => [FilePath] -> Evaluator address value effects (Maybe ModulePath)
resolve = sendModules . Resolve @address @value

listModulesInDir :: forall address value effects . Member (Modules address value) effects => FilePath -> Evaluator address value effects [ModulePath]
listModulesInDir = sendModules . List @address @value


-- | Require/import another module by name and return its environment and value.
--
-- Looks up the module's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: Member (Modules address value) effects => ModulePath -> Evaluator address value effects (Maybe (address, Environment address))
require path = lookupModule path >>= maybeM (load path)

-- | Load another module by name and return its environment and value.
--
-- Always loads/evaluates.
load :: Member (Modules address value) effects => ModulePath -> Evaluator address value effects (Maybe (address, Environment address))
load path = sendModules (Load path)


data Modules address value return where
  Load    :: ModulePath -> Modules address value (Maybe (address, Environment address))
  Lookup  :: ModulePath -> Modules address value (Maybe (Maybe (address, Environment address)))
  Resolve :: [FilePath] -> Modules address value (Maybe ModulePath)
  List    :: FilePath   -> Modules address value [ModulePath]

sendModules :: Member (Modules address value) effects => Modules address value return -> Evaluator address value effects return
sendModules = send

runModules :: forall term address value effects a
           .  ( Member (Resumable (LoadError address value)) effects
              , Member (State (ModuleTable (Maybe (address, Environment address)))) effects
              , Member Trace effects
              )
           => (Module term -> Evaluator address value (Modules address value ': effects) (address, Environment address))
           -> Evaluator address value (Modules address value ': effects) a
           -> Evaluator address value (Reader (ModuleTable (NonEmpty (Module term))) ': effects) a
runModules evaluateModule = go
  where go :: forall a . Evaluator address value (Modules address value ': effects) a -> Evaluator address value (Reader (ModuleTable (NonEmpty (Module term))) ': effects) a
        go = reinterpret (\ m -> case m of
          Load name -> askModuleTable @term >>= maybe (moduleNotFound name) (runMerging . foldMap (Merging . evalAndCache)) . ModuleTable.lookup name
            where
              evalAndCache x = do
                let mPath = modulePath (moduleInfo x)
                loading <- loadingModule mPath
                if loading
                  then trace ("load (skip evaluating, circular load): " <> show mPath) $> Nothing
                  else do
                    _ <- cacheModule name Nothing
                    result <- trace ("load (evaluating): " <> show mPath) *> go (evaluateModule x) <* trace ("load done:" <> show mPath)
                    cacheModule name (Just result)

              loadingModule path = isJust . ModuleTable.lookup path <$> getModuleTable
          Lookup path -> ModuleTable.lookup path <$> get
          Resolve names -> do
            isMember <- flip ModuleTable.member <$> askModuleTable @term
            pure (find isMember names)
          List dir -> modulePathsInDir dir <$> askModuleTable @term)

getModuleTable :: Member (State (ModuleTable (Maybe (address, Environment address)))) effects => Evaluator address value effects (ModuleTable (Maybe (address, Environment address)))
getModuleTable = get

cacheModule :: Member (State (ModuleTable (Maybe (address, Environment address)))) effects => ModulePath -> Maybe (address, Environment address) -> Evaluator address value effects (Maybe (address, Environment address))
cacheModule path result = modify' (ModuleTable.insert path result) $> result

askModuleTable :: Member (Reader (ModuleTable (NonEmpty (Module term)))) effects => Evaluator address value effects (ModuleTable (NonEmpty (Module term)))
askModuleTable = ask


newtype Merging m address value = Merging { runMerging :: m (Maybe (address, Environment address)) }

instance Applicative m => Semigroup (Merging m address value) where
  Merging a <> Merging b = Merging (merge <$> a <*> b)
    where merge a b = mergeJusts <$> a <*> b <|> a <|> b
          mergeJusts (_, env1) (v, env2) = (v, mergeEnvs env1 env2)

instance Applicative m => Monoid (Merging m address value) where
  mappend = (<>)
  mempty = Merging (pure Nothing)


-- | An error thrown when loading a module from the list of provided modules. Indicates we weren't able to find a module with the given name.
data LoadError address value resume where
  ModuleNotFound :: ModulePath -> LoadError address value (Maybe (address, Environment address))

deriving instance Eq (LoadError address value resume)
deriving instance Show (LoadError address value resume)
instance Show1 (LoadError address value) where
  liftShowsPrec _ _ = showsPrec
instance Eq1 (LoadError address value) where
  liftEq _ (ModuleNotFound a) (ModuleNotFound b) = a == b

moduleNotFound :: forall address value effects . Member (Resumable (LoadError address value)) effects => ModulePath -> Evaluator address value effects (Maybe (address, Environment address))
moduleNotFound = throwResumable . ModuleNotFound @address @value

resumeLoadError :: Member (Resumable (LoadError address value)) effects => Evaluator address value effects a -> (forall resume . LoadError address value resume -> Evaluator address value effects resume) -> Evaluator address value effects a
resumeLoadError = catchResumable

runLoadError :: Effectful (m address value) => m address value (Resumable (LoadError address value) ': effects) a -> m address value effects (Either (SomeExc (LoadError address value)) a)
runLoadError = runResumable

runLoadErrorWith :: Effectful (m address value) => (forall resume . LoadError address value resume -> m address value effects resume) -> m address value (Resumable (LoadError address value) ': effects) a -> m address value effects a
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
runResolutionError = runResumable

runResolutionErrorWith :: Effectful m => (forall resume . ResolutionError resume -> m effects resume) -> m (Resumable ResolutionError ': effects) a -> m effects a
runResolutionErrorWith = runResumableWith
