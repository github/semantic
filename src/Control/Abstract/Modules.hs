{-# LANGUAGE GADTs, LambdaCase, KindSignatures, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Control.Abstract.Modules
( ModuleResult
, lookupModule
, resolve
, listModulesInDir
, require
, load
, Modules(..)
, runModules
, LoadError(..)
, runLoadError
, runLoadErrorWith
, throwLoadError
, ResolutionError(..)
, runResolutionError
, runResolutionErrorWith
, throwResolutionError
, ModuleTable
) where

import Control.Abstract.Evaluator
import Data.Abstract.Environment
import Data.Abstract.BaseError
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Language
import Data.Semigroup.Foldable (foldMap1)
import qualified Data.Set as Set
import Data.Span
import Prologue
import System.FilePath.Posix (takeDirectory)

type ModuleResult address = (Bindings address, address)

-- | Retrieve an evaluated module, if any. @Nothing@ means we’ve never tried to load it, and @Just (env, value)@ indicates the result of a completed load.
lookupModule :: Member (Modules address) effects => ModulePath -> Evaluator address value effects (Maybe (ModuleResult address))
lookupModule = sendModules . Lookup

-- | Resolve a list of module paths to a possible module table entry.
resolve :: Member (Modules address) effects => [FilePath] -> Evaluator address value effects (Maybe ModulePath)
resolve = sendModules . Resolve

listModulesInDir :: Member (Modules address) effects => FilePath -> Evaluator address value effects [ModulePath]
listModulesInDir = sendModules . List


-- | Require/import another module by name and return its environment and value.
--
-- Looks up the module's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: Member (Modules address) effects => ModulePath -> Evaluator address value effects (ModuleResult address)
require path = lookupModule path >>= maybeM (load path)

-- | Load another module by name and return its environment and value.
--
-- Always loads/evaluates.
load :: Member (Modules address) effects => ModulePath -> Evaluator address value effects (ModuleResult address)
load path = sendModules (Load path)


data Modules address (m :: * -> *) return where
  Load    :: ModulePath -> Modules address m (ModuleResult address)
  Lookup  :: ModulePath -> Modules address m (Maybe (ModuleResult address))
  Resolve :: [FilePath] -> Modules address m (Maybe ModulePath)
  List    :: FilePath   -> Modules address m [ModulePath]

instance PureEffect (Modules address)
instance Effect (Modules address) where
  handleState c dist (Request (Load path) k) = Request (Load path) (dist . (<$ c) . k)
  handleState c dist (Request (Lookup path) k) = Request (Lookup path) (dist . (<$ c) . k)
  handleState c dist (Request (Resolve paths) k) = Request (Resolve paths) (dist . (<$ c) . k)
  handleState c dist (Request (List path) k) = Request (List path) (dist . (<$ c) . k)

sendModules :: Member (Modules address) effects => Modules address (Eff effects) return -> Evaluator address value effects return
sendModules = send

runModules :: ( Member (Reader (ModuleTable (NonEmpty (Module (ModuleResult address))))) effects
              , Member (Resumable (BaseError (LoadError address))) effects
              , PureEffects effects
              )
           => Set ModulePath
           -> Evaluator address value (Modules address ': effects) a
           -> Evaluator address value effects a
runModules paths = interpret $ \case
  Load   name   -> fmap (runMerging . foldMap1 (Merging . moduleBody)) . ModuleTable.lookup name <$> askModuleTable >>= maybeM (throwLoadError (ModuleNotFoundError name))
  Lookup path   -> fmap (runMerging . foldMap1 (Merging . moduleBody)) . ModuleTable.lookup path <$> askModuleTable
  Resolve names -> pure (find (`Set.member` paths) names)
  List dir      -> pure (filter ((dir ==) . takeDirectory) (toList paths))

askModuleTable :: Member (Reader (ModuleTable (NonEmpty (Module (ModuleResult address))))) effects => Evaluator address value effects (ModuleTable (NonEmpty (Module (ModuleResult address))))
askModuleTable = ask


newtype Merging address = Merging { runMerging :: ModuleResult address }

instance Semigroup (Merging address) where
  Merging (binds1, _) <> Merging (binds2, addr) = Merging (binds1 <> binds2, addr)


-- | An error thrown when loading a module from the list of provided modules. Indicates we weren't able to find a module with the given name.
data LoadError address resume where
  ModuleNotFoundError :: ModulePath -> LoadError address (ModuleResult address)

deriving instance Eq (LoadError address resume)
deriving instance Show (LoadError address resume)
instance Show1 (LoadError address) where
  liftShowsPrec _ _ = showsPrec
instance Eq1 (LoadError address) where
  liftEq _ (ModuleNotFoundError a) (ModuleNotFoundError b) = a == b

runLoadError :: (Effectful (m address value), Effects effects)
             => m address value (Resumable (BaseError (LoadError address)) ': effects) a
             -> m address value effects (Either (SomeExc (BaseError (LoadError address))) a)
runLoadError = runResumable

runLoadErrorWith :: (Effectful (m address value), Effects effects)
                 => (forall resume . (BaseError (LoadError address)) resume -> m address value effects resume)
                 -> m address value (Resumable (BaseError (LoadError address)) ': effects) a
                 -> m address value effects a
runLoadErrorWith = runResumableWith

throwLoadError :: Member (Resumable (BaseError (LoadError address))) effects
               => LoadError address resume
               -> Evaluator address value effects resume
throwLoadError err@(ModuleNotFoundError name) = throwResumable $ BaseError (ModuleInfo name) emptySpan err


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

runResolutionError :: (Effectful m, Effects effects)
                   => m (Resumable (BaseError ResolutionError) ': effects) a
                   -> m effects (Either (SomeExc (BaseError ResolutionError)) a)
runResolutionError = runResumable

runResolutionErrorWith :: (Effectful m, Effects effects)
                       => (forall resume . (BaseError ResolutionError) resume -> m effects resume)
                       -> m (Resumable (BaseError ResolutionError) ': effects) a
                       -> m effects a
runResolutionErrorWith = runResumableWith

throwResolutionError :: ( Member (Reader ModuleInfo) effects
                        , Member (Reader Span) effects
                        , Member (Resumable (BaseError ResolutionError)) effects
                        )
                     => ResolutionError resume
                     -> Evaluator address value effects resume
throwResolutionError = throwBaseError
