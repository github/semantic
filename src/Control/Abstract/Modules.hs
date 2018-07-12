{-# LANGUAGE GADTs, LambdaCase, KindSignatures, RankNTypes, ScopedTypeVariables, TypeOperators #-}
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
import Data.Semigroup.Foldable (foldMap1)
import qualified Data.Set as Set
import Prologue
import System.FilePath.Posix (takeDirectory)

-- | Retrieve an evaluated module, if any. @Nothing@ means we’ve never tried to load it, and @Just (env, value)@ indicates the result of a completed load.
lookupModule :: Member (Modules address) effects => ModulePath -> Evaluator address value effects (Maybe (Environment address, address))
lookupModule = sendModules . Lookup

-- | Resolve a list of module paths to a possible module table entry.
resolve :: Member (Modules address) effects => [FilePath] -> Evaluator address value effects (Maybe ModulePath)
resolve = sendModules . Resolve

listModulesInDir :: Member (Modules address) effects => FilePath -> Evaluator address value effects [ModulePath]
listModulesInDir = sendModules . List


-- | Require/import another module by name and return its environment and value.
--
-- Looks up the module's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: Member (Modules address) effects => ModulePath -> Evaluator address value effects (Environment address, address)
require path = lookupModule path >>= maybeM (load path)

-- | Load another module by name and return its environment and value.
--
-- Always loads/evaluates.
load :: Member (Modules address) effects => ModulePath -> Evaluator address value effects (Environment address, address)
load path = sendModules (Load path)


data Modules address (m :: * -> *) return where
  Load    :: ModulePath -> Modules address m (Environment address, address)
  Lookup  :: ModulePath -> Modules address m (Maybe (Environment address, address))
  Resolve :: [FilePath] -> Modules address m (Maybe ModulePath)
  List    :: FilePath   -> Modules address m [ModulePath]

instance Effect (Modules address) where
  handleState c dist (Load path) k = Request (Load path) (\result -> dist (pure result <$ c) k)
  handleState c dist (Lookup path) k = Request (Lookup path) (\result -> dist (pure result <$ c) k)
  handleState c dist (Resolve paths) k = Request (Resolve paths) (\result -> dist (pure result <$ c) k)
  handleState c dist (List path) k = Request (List path) (\result -> dist (pure result <$ c) k)

sendModules :: Member (Modules address) effects => Modules address (Eff effects) return -> Evaluator address value effects return
sendModules = send

runModules :: ( Effects effects
              , Member (Reader (ModuleTable (NonEmpty (Module (Environment address, address))))) effects
              , Member (Resumable (LoadError address)) effects
              )
           => Set ModulePath
           -> Evaluator address value (Modules address ': effects) a
           -> Evaluator address value effects a
runModules paths = interpret $ \case
  Load   name   -> fmap (runMerging . foldMap1 (Merging . moduleBody)) . ModuleTable.lookup name <$> askModuleTable >>= maybeM (moduleNotFound name)
  Lookup path   -> fmap (runMerging . foldMap1 (Merging . moduleBody)) . ModuleTable.lookup path <$> askModuleTable
  Resolve names -> pure (find (`Set.member` paths) names)
  List dir      -> pure (filter ((dir ==) . takeDirectory) (toList paths))

askModuleTable :: Member (Reader (ModuleTable (NonEmpty (Module (Environment address, address))))) effects => Evaluator address value effects (ModuleTable (NonEmpty (Module (Environment address, address))))
askModuleTable = ask


newtype Merging address = Merging { runMerging :: (Environment address, address) }

instance Semigroup (Merging address) where
  Merging (env1, _) <> Merging (env2, addr) = Merging (mergeEnvs env1 env2, addr)


-- | An error thrown when loading a module from the list of provided modules. Indicates we weren't able to find a module with the given name.
data LoadError address resume where
  ModuleNotFound :: ModulePath -> LoadError address (Environment address, address)

deriving instance Eq (LoadError address resume)
deriving instance Show (LoadError address resume)
instance Show1 (LoadError address) where
  liftShowsPrec _ _ = showsPrec
instance Eq1 (LoadError address) where
  liftEq _ (ModuleNotFound a) (ModuleNotFound b) = a == b

moduleNotFound :: Member (Resumable (LoadError address)) effects => ModulePath -> Evaluator address value effects (Environment address, address)
moduleNotFound = throwResumable . ModuleNotFound

runLoadError :: (Effectful (m address value), Effects effects) => m address value (Resumable (LoadError address) ': effects) a -> m address value effects (Either (SomeExc (LoadError address)) a)
runLoadError = runResumable

runLoadErrorWith :: (Effectful (m address value), Effects effects) => (forall resume . LoadError address resume -> m address value effects resume) -> m address value (Resumable (LoadError address) ': effects) a -> m address value effects a
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
