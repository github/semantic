{-# LANGUAGE GADTs, LambdaCase, RankNTypes, ScopedTypeVariables, TypeOperators #-}
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
import Data.Semigroup.Foldable (foldMap1)
import qualified Data.Set as Set
import Prologue
import System.FilePath.Posix (takeDirectory)

-- | Retrieve an evaluated module, if any. @Nothing@ means we’ve never tried to load it, and @Just (env, value)@ indicates the result of a completed load.
lookupModule :: Member (Modules address) effects => ModulePath -> Evaluator address value effects (Maybe (address, Environment address))
lookupModule = sendModules . Lookup

-- | Resolve a list of module paths to a possible module table entry.
resolve :: Member (Modules address) effects => [FilePath] -> Evaluator address value effects (Maybe ModulePath)
resolve = sendModules . Resolve

listModulesInDir :: Member (Modules address) effects => FilePath -> Evaluator address value effects [ModulePath]
listModulesInDir = sendModules . List


-- | Require/import another module by name and return its environment and value.
--
-- Looks up the module's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: Member (Modules address) effects => ModulePath -> Evaluator address value effects (address, Environment address)
require path = lookupModule path >>= maybeM (load path)

-- | Load another module by name and return its environment and value.
--
-- Always loads/evaluates.
load :: Member (Modules address) effects => ModulePath -> Evaluator address value effects (address, Environment address)
load path = sendModules (Load path)


data Modules address return where
  Load    :: ModulePath -> Modules address (address, Environment address)
  Lookup  :: ModulePath -> Modules address (Maybe (address, Environment address))
  Resolve :: [FilePath] -> Modules address (Maybe ModulePath)
  List    :: FilePath   -> Modules address [ModulePath]

sendModules :: Member (Modules address) effects => Modules address return -> Evaluator address value effects return
sendModules = send

runModules :: ( Member (Reader (ModuleTable (NonEmpty (Module (address, Environment address))))) effects
              , Member (Resumable (LoadError address)) effects
              )
           => Set ModulePath
           -> Evaluator address value (Modules address ': effects) a
           -> Evaluator address value effects a
runModules paths = interpret $ \case
  Load   name   -> fmap (runMerging . foldMap1 (Merging . moduleBody)) . ModuleTable.lookup name <$> askModuleTable >>= maybeM (moduleNotFound name)
  Lookup path   -> fmap (runMerging . foldMap1 (Merging . moduleBody)) . ModuleTable.lookup path <$> askModuleTable
  Resolve names -> pure (find (flip Set.member paths) names)
  List dir      -> pure (filter ((dir ==) . takeDirectory) (toList paths))

askModuleTable :: Member (Reader (ModuleTable (NonEmpty (Module (address, Environment address))))) effects => Evaluator address value effects (ModuleTable (NonEmpty (Module (address, Environment address))))
askModuleTable = ask


newtype Merging address = Merging { runMerging :: (address, Environment address) }

instance Semigroup (Merging address) where
  Merging (_, env1) <> Merging (addr, env2) = Merging (addr, mergeEnvs env1 env2)


-- | An error thrown when loading a module from the list of provided modules. Indicates we weren't able to find a module with the given name.
data LoadError address resume where
  ModuleNotFound :: ModulePath -> LoadError address (address, Environment address)

deriving instance Eq (LoadError address resume)
deriving instance Show (LoadError address resume)
instance Show1 (LoadError address) where
  liftShowsPrec _ _ = showsPrec
instance Eq1 (LoadError address) where
  liftEq _ (ModuleNotFound a) (ModuleNotFound b) = a == b

moduleNotFound :: Member (Resumable (LoadError address)) effects => ModulePath -> Evaluator address value effects (address, Environment address)
moduleNotFound = throwResumable . ModuleNotFound

resumeLoadError :: Member (Resumable (LoadError address)) effects => Evaluator address value effects a -> (forall resume . LoadError address resume -> Evaluator address value effects resume) -> Evaluator address value effects a
resumeLoadError = catchResumable

runLoadError :: Effectful (m address value) => m address value (Resumable (LoadError address) ': effects) a -> m address value effects (Either (SomeExc (LoadError address)) a)
runLoadError = runResumable

runLoadErrorWith :: Effectful (m address value) => (forall resume . LoadError address resume -> m address value effects resume) -> m address value (Resumable (LoadError address) ': effects) a -> m address value effects a
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
