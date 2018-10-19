{-# LANGUAGE GADTs, LambdaCase, KindSignatures, RankNTypes, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
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
import Data.Coerce
import Data.Language
import Data.Semigroup.Foldable (foldMap1)
import qualified Data.Set as Set
import Data.Span
import Prologue
import System.FilePath.Posix (takeDirectory)
import Data.Abstract.ScopeGraph

type ModuleResult address = (ScopeGraph address, (Bindings address, address))

-- | Retrieve an evaluated module, if any. @Nothing@ means we’ve never tried to load it, and @Just (env, value)@ indicates the result of a completed load.
lookupModule :: (Member (Modules address) sig, Carrier sig m) => ModulePath -> Evaluator term address value m (Maybe (ModuleResult address))
lookupModule = sendModules . flip Lookup gen

-- | Resolve a list of module paths to a possible module table entry.
resolve :: (Member (Modules address) sig, Carrier sig m) => [FilePath] -> Evaluator term address value m (Maybe ModulePath)
resolve = sendModules . flip Resolve gen

listModulesInDir :: (Member (Modules address) sig, Carrier sig m) => FilePath -> Evaluator term address value m [ModulePath]
listModulesInDir = sendModules . flip List gen


-- | Require/import another module by name and return its environment and value.
--
-- Looks up the module's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: (Member (Modules address) sig, Carrier sig m) => ModulePath -> Evaluator term address value m (ModuleResult address)
require path = lookupModule path >>= maybeM (load path)

-- | Load another module by name and return its environment and value.
--
-- Always loads/evaluates.
load :: (Member (Modules address) sig, Carrier sig m) => ModulePath -> Evaluator term address value m (ModuleResult address)
load path = sendModules (Load path gen)


data Modules address (m :: * -> *) k
  = Load    ModulePath (ModuleResult address -> k)
  | Lookup  ModulePath (Maybe (ModuleResult address) -> k)
  | Resolve [FilePath] (Maybe ModulePath -> k)
  | List    FilePath   ([ModulePath] -> k)
  deriving (Functor)

instance HFunctor (Modules address) where
  hmap _ = coerce

instance Effect (Modules address) where
  handle state handler (Load    path  k) = Load    path  (handler . (<$ state) . k)
  handle state handler (Lookup  path  k) = Lookup  path  (handler . (<$ state) . k)
  handle state handler (Resolve paths k) = Resolve paths (handler . (<$ state) . k)
  handle state handler (List    path  k) = List    path  (handler . (<$ state) . k)

sendModules :: (Member (Modules address) sig, Carrier sig m) => Modules address (Evaluator term address value m) (Evaluator term address value m return) -> Evaluator term address value m return
sendModules = send

runModules :: ( Member (Reader (ModuleTable (NonEmpty (Module (ModuleResult address))))) sig
              , Member (Resumable (BaseError (LoadError address))) sig
              , Carrier sig m
              )
           => Set ModulePath
           -> Evaluator term address value (ModulesC
             (Evaluator term address value m)) a
           -> Evaluator term address value m a
runModules paths = flip runModulesC paths . interpret . runEvaluator

newtype ModulesC m a = ModulesC { runModulesC :: Set ModulePath -> m a }

instance ( Member (Reader (ModuleTable (NonEmpty (Module (ModuleResult address))))) sig
         , Member (Resumable (BaseError (LoadError address))) sig
         , Carrier sig m
         )
      => Carrier (Modules address :+: sig) (ModulesC (Evaluator term address value m)) where
  gen = ModulesC . const . gen
  alg op = ModulesC (\ paths -> (algM paths \/ (alg . handlePure (flip runModulesC paths))) op)
    where algM paths (Load    name  k) = askModuleTable >>= maybeM (throwLoadError (ModuleNotFoundError name)) . fmap (runMerging . foldMap1 (Merging . moduleBody)) . ModuleTable.lookup name >>= flip runModulesC paths . k
          algM paths (Lookup  path  k) = askModuleTable >>= flip runModulesC paths . k . fmap (runMerging . foldMap1 (Merging . moduleBody)) . ModuleTable.lookup path
          algM paths (Resolve names k) = runModulesC (k (find (`Set.member` paths) names)) paths
          algM paths (List    dir   k) = runModulesC (k (filter ((dir ==) . takeDirectory) (toList paths))) paths

askModuleTable :: (Member (Reader (ModuleTable (NonEmpty (Module (ModuleResult address))))) sig, Carrier sig m) => Evaluator term address value m (ModuleTable (NonEmpty (Module (ModuleResult address))))
askModuleTable = ask


newtype Merging address = Merging { runMerging :: ModuleResult address }

instance Semigroup (Merging address) where
  Merging (_, (binds1, _)) <> Merging (graph2, (binds2, addr)) = Merging (graph2, (binds1 <> binds2, addr))


-- | An error thrown when loading a module from the list of provided modules. Indicates we weren't able to find a module with the given name.
data LoadError address resume where
  ModuleNotFoundError :: ModulePath -> LoadError address (ModuleResult address)

deriving instance Eq (LoadError address resume)
deriving instance Show (LoadError address resume)
instance Show1 (LoadError address) where
  liftShowsPrec _ _ = showsPrec
instance Eq1 (LoadError address) where
  liftEq _ (ModuleNotFoundError a) (ModuleNotFoundError b) = a == b

runLoadError :: (Carrier sig m, Effect sig)
             => Evaluator term address value (ResumableC (BaseError (LoadError address)) (Evaluator term address value m)) a
             -> Evaluator term address value m (Either (SomeError (BaseError (LoadError address))) a)
runLoadError = runResumable . runEvaluator

runLoadErrorWith :: Carrier sig m
                 => (forall resume . (BaseError (LoadError address)) resume -> Evaluator term address value m resume)
                 -> Evaluator term address value (ResumableWithC (BaseError (LoadError address)) (Evaluator term address value m)) a
                 -> Evaluator term address value m a
runLoadErrorWith f = runResumableWith f . runEvaluator

throwLoadError :: (Member (Resumable (BaseError (LoadError address))) sig, Carrier sig m)
               => LoadError address resume
               -> Evaluator term address value m resume
throwLoadError err@(ModuleNotFoundError name) = throwResumable $ BaseError (ModuleInfo name) emptySpan err


-- | An error thrown when we can't resolve a module from a qualified name.
data ResolutionError resume where
  NotFoundError :: String   -- The path that was not found.
                -> [String] -- List of paths searched that shows where semantic looked for this module.
                -> Language -- Language.
                -> ResolutionError ModulePath

  GoImportError :: FilePath -> ResolutionError [ModulePath]

deriving instance Eq (ResolutionError b)
deriving instance Show (ResolutionError b)
instance Show1 ResolutionError where liftShowsPrec _ _ = showsPrec
instance Eq1 ResolutionError where
  liftEq _ (NotFoundError a _ l1) (NotFoundError b _ l2) = a == b && l1 == l2
  liftEq _ (GoImportError a) (GoImportError b) = a == b
  liftEq _ _ _ = False

runResolutionError :: (Carrier sig m, Effect sig)
                   => Evaluator term address value (ResumableC (BaseError ResolutionError) (Evaluator term address value m)) a
                   -> Evaluator term address value m (Either (SomeError (BaseError ResolutionError)) a)
runResolutionError = runResumable . runEvaluator

runResolutionErrorWith :: Carrier sig m
                       => (forall resume . (BaseError ResolutionError) resume -> Evaluator term address value m resume)
                       -> Evaluator term address value (ResumableWithC (BaseError ResolutionError) (Evaluator term address value m)) a
                       -> Evaluator term address value m a
runResolutionErrorWith f = runResumableWith f . runEvaluator

throwResolutionError :: ( Member (Reader ModuleInfo) sig
                        , Member (Reader Span) sig
                        , Member (Resumable (BaseError ResolutionError)) sig
                        , Carrier sig m
                        )
                     => ResolutionError resume
                     -> Evaluator term address value m resume
throwResolutionError = throwBaseError
