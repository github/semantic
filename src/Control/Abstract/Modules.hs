{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, TypeOperators #-}
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
lookupModule :: Member (Modules address value) effects => ModulePath -> Evaluator address value effects (Maybe (Maybe (Environment address, address)))
lookupModule = sendModules . Lookup

-- | Resolve a list of module paths to a possible module table entry.
resolve :: forall address value effects . Member (Modules address value) effects => [FilePath] -> Evaluator address value effects (Maybe ModulePath)
resolve = sendModules . Resolve @address @value

listModulesInDir :: forall address value effects . Member (Modules address value) effects => FilePath -> Evaluator address value effects [ModulePath]
listModulesInDir = sendModules . List @address @value


-- | Require/import another module by name and return its environment and value.
--
-- Looks up the module's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: Member (Modules address value) effects => ModulePath -> Evaluator address value effects (Maybe (Environment address, address))
require path = lookupModule path >>= maybeM (load path)

-- | Load another module by name and return its environment and value.
--
-- Always loads/evaluates.
load :: Member (Modules address value) effects => ModulePath -> Evaluator address value effects (Maybe (Environment address, address))
load path = sendModules (Load path)


data Modules address value (m :: * -> *) return where
  Load    :: ModulePath -> Modules address value m (Maybe (Environment address, address))
  Lookup  :: ModulePath -> Modules address value m (Maybe (Maybe (Environment address, address)))
  Resolve :: [FilePath] -> Modules address value m (Maybe ModulePath)
  List    :: FilePath   -> Modules address value m [ModulePath]

instance Effect (Modules address value) where
  handleState c dist (Request (Load path) k) = Request (Load path) (dist . (<$ c) . k)
  handleState c dist (Request (Lookup path) k) = Request (Lookup path) (dist . (<$ c) . k)
  handleState c dist (Request (Resolve paths) k) = Request (Resolve paths) (dist . (<$ c) . k)
  handleState c dist (Request (List path) k) = Request (List path) (dist . (<$ c) . k)


sendModules :: Member (Modules address value) effects => Modules address value (Eff effects) return -> Evaluator address value effects return
sendModules = send

runModules :: forall term address value effects a
           .  ( Member (Resumable (LoadError address value)) effects
              , Member (State (ModuleTable (Maybe (Environment address, address)))) effects
              , Member Trace effects
              , Effects effects
              )
           => (Module term -> Evaluator address value (Modules address value ': effects) (Environment address, address))
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

getModuleTable :: Member (State (ModuleTable (Maybe (Environment address, address)))) effects => Evaluator address value effects (ModuleTable (Maybe (Environment address, address)))
getModuleTable = get

cacheModule :: Member (State (ModuleTable (Maybe (Environment address, address)))) effects => ModulePath -> Maybe (Environment address, address) -> Evaluator address value effects (Maybe (Environment address, address))
cacheModule path result = modify' (ModuleTable.insert path result) $> result

askModuleTable :: Member (Reader (ModuleTable (NonEmpty (Module term)))) effects => Evaluator address value effects (ModuleTable (NonEmpty (Module term)))
askModuleTable = ask


newtype Merging m address value = Merging { runMerging :: m (Maybe (Environment address, address)) }

instance Applicative m => Semigroup (Merging m address value) where
  Merging a <> Merging b = Merging (merge <$> a <*> b)
    where merge a b = mergeJusts <$> a <*> b <|> a <|> b
          mergeJusts (env1, _) (env2, v) = (mergeEnvs env1 env2, v)

instance Applicative m => Monoid (Merging m address value) where
  mappend = (<>)
  mempty = Merging (pure Nothing)


-- | An error thrown when loading a module from the list of provided modules. Indicates we weren't able to find a module with the given name.
data LoadError address value resume where
  ModuleNotFound :: ModulePath -> LoadError address value (Maybe (Environment address, address))

deriving instance Eq (LoadError address value resume)
deriving instance Show (LoadError address value resume)
instance Show1 (LoadError address value) where
  liftShowsPrec _ _ = showsPrec
instance Eq1 (LoadError address value) where
  liftEq _ (ModuleNotFound a) (ModuleNotFound b) = a == b

moduleNotFound :: forall address value effects . Member (Resumable (LoadError address value)) effects => ModulePath -> Evaluator address value effects (Maybe (Environment address, address))
moduleNotFound = throwResumable . ModuleNotFound @address @value

runLoadError :: (Effectful (m address value), Effects effects) => m address value (Resumable (LoadError address value) ': effects) a -> m address value effects (Either (SomeExc (LoadError address value)) a)
runLoadError = runResumable

runLoadErrorWith :: (Effectful (m address value), Effects effects) => (forall resume . LoadError address value resume -> m address value effects resume) -> m address value (Resumable (LoadError address value) ': effects) a -> m address value effects a
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

runResolutionError :: (Effectful m, Effects effects) => m (Resumable ResolutionError ': effects) a -> m effects (Either (SomeExc ResolutionError) a)
runResolutionError = runResumable

runResolutionErrorWith :: (Effectful m, Effects effects) => (forall resume . ResolutionError resume -> m effects resume) -> m (Resumable ResolutionError ': effects) a -> m effects a
runResolutionErrorWith = runResumableWith
