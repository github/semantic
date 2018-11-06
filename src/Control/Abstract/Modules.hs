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
import Control.Effect.Carrier
import Control.Effect.Sum
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
import Data.Abstract.Ref

type ModuleResult address value = (ScopeGraph address, ValueRef address value)

-- | Retrieve an evaluated module, if any. @Nothing@ means we’ve never tried to load it, and @Just (env, value)@ indicates the result of a completed load.
lookupModule :: (Member (Modules address value) sig, Carrier sig m) => ModulePath -> Evaluator term address value m (Maybe (ModuleResult address value))
lookupModule = sendModules . flip Lookup ret

-- | Resolve a list of module paths to a possible module table entry.
resolve :: (Member (Modules address value) sig, Carrier sig m) => [FilePath] -> Evaluator term address value m (Maybe ModulePath)
resolve = sendModules . flip Resolve ret

listModulesInDir :: (Member (Modules address value) sig, Carrier sig m) => FilePath -> Evaluator term address value m [ModulePath]
listModulesInDir = sendModules . flip List ret


-- | Require/import another module by name and return its environment and value.
--
-- Looks up the module's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: (Member (Modules address value) sig, Carrier sig m) => ModulePath -> Evaluator term address value m (ModuleResult address value)
require path = lookupModule path >>= maybeM (load path)

-- | Load another module by name and return its environment and value.
--
-- Always loads/evaluates.
load :: (Member (Modules address value) sig, Carrier sig m) => ModulePath -> Evaluator term address value m (ModuleResult address value)
load path = sendModules (Load path ret)


data Modules address value (m :: * -> *) k
  = Load    ModulePath (ModuleResult address value -> k)
  | Lookup  ModulePath (Maybe (ModuleResult address value) -> k)
  | Resolve [FilePath] (Maybe ModulePath -> k)
  | List    FilePath   ([ModulePath] -> k)
  deriving (Functor)

instance HFunctor (Modules address value) where
  hmap _ = coerce

instance Effect (Modules address value) where
  handle state handler (Load    path  k) = Load    path  (handler . (<$ state) . k)
  handle state handler (Lookup  path  k) = Lookup  path  (handler . (<$ state) . k)
  handle state handler (Resolve paths k) = Resolve paths (handler . (<$ state) . k)
  handle state handler (List    path  k) = List    path  (handler . (<$ state) . k)


sendModules :: ( Member (Modules address value) sig
               , Carrier sig m)
            => Modules address value (Evaluator term address value m) (Evaluator term address value m return)
            -> Evaluator term address value m return
sendModules = send

runModules :: ( Member (Reader (ModuleTable (NonEmpty (Module (ModuleResult address value))))) sig
              , Member (Resumable (BaseError (LoadError address value))) sig
              , Carrier sig m
              )
           => Set ModulePath
           -> Evaluator term address value (ModulesC address value (Eff m)) a
           -> Evaluator term address value m a
runModules paths = raiseHandler $ flip runModulesC paths . interpret

newtype ModulesC address value m a = ModulesC { runModulesC :: Set ModulePath -> m a }

instance ( Member (Reader (ModuleTable (NonEmpty (Module (ModuleResult address value))))) sig
         , Member (Resumable (BaseError (LoadError address value))) sig
         , Carrier sig m
         , Monad m
         )
      => Carrier (Modules address value :+: sig) (ModulesC address value m) where
  ret = ModulesC . const . ret
  eff op = ModulesC (\ paths -> handleSum (eff . handleReader paths runModulesC) (\case
    Load    name  k -> askModuleTable >>= maybeM (throwLoadError (ModuleNotFoundError name)) . fmap (runMerging . foldMap1 (Merging . moduleBody)) . ModuleTable.lookup name >>= flip runModulesC paths . k
    Lookup  path  k -> askModuleTable >>= flip runModulesC paths . k . fmap (runMerging . foldMap1 (Merging . moduleBody)) . ModuleTable.lookup path
    Resolve names k -> runModulesC (k (find (`Set.member` paths) names)) paths
    List    dir   k -> runModulesC (k (filter ((dir ==) . takeDirectory) (toList paths))) paths) op)

askModuleTable :: (Member (Reader (ModuleTable (NonEmpty (Module (ModuleResult address value))))) sig, Carrier sig m) => m (ModuleTable (NonEmpty (Module (ModuleResult address value))))
askModuleTable = ask


newtype Merging address value = Merging { runMerging :: ModuleResult address value }

instance Semigroup (Merging address value) where
  -- TODO: We may need to combine graphs
  Merging (_, _) <> Merging (graph2, addr) = Merging (graph2, addr)


-- | An error thrown when loading a module from the list of provided modules. Indicates we weren't able to find a module with the given name.
data LoadError address value resume where
  ModuleNotFoundError :: ModulePath -> LoadError address value (ModuleResult address value)

deriving instance Eq (LoadError address value resume)
deriving instance Show (LoadError address value resume)
instance Show1 (LoadError address value) where
  liftShowsPrec _ _ = showsPrec
instance Eq1 (LoadError address value) where
  liftEq _ (ModuleNotFoundError a) (ModuleNotFoundError b) = a == b

instance NFData1 (LoadError address value) where
  liftRnf _ (ModuleNotFoundError p) = rnf p

runLoadError :: (Carrier sig m, Effect sig)
             => Evaluator term address value (ResumableC (BaseError (LoadError address value)) (Eff m)) a
             -> Evaluator term address value m (Either (SomeError (BaseError (LoadError address value))) a)
runLoadError = raiseHandler runResumable

runLoadErrorWith :: Carrier sig m
                 => (forall resume . (BaseError (LoadError address value)) resume -> Evaluator term address value m resume)
                 -> Evaluator term address value (ResumableWithC (BaseError (LoadError address value)) (Eff m)) a
                 -> Evaluator term address value m a
runLoadErrorWith f = raiseHandler $ runResumableWith (runEvaluator . f)

throwLoadError :: (Member (Resumable (BaseError (LoadError address value))) sig, Carrier sig m)
               => LoadError address value resume
               -> m resume
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
instance NFData1 ResolutionError where
  liftRnf _ x = case x of
    NotFoundError p ps l -> rnf p `seq` rnf ps `seq` rnf l
    GoImportError p      -> rnf p

runResolutionError :: (Carrier sig m, Effect sig)
                   => Evaluator term address value (ResumableC (BaseError ResolutionError) (Eff m)) a
                   -> Evaluator term address value m (Either (SomeError (BaseError ResolutionError)) a)
runResolutionError = raiseHandler runResumable

runResolutionErrorWith :: Carrier sig m
                       => (forall resume . (BaseError ResolutionError) resume -> Evaluator term address value m resume)
                       -> Evaluator term address value (ResumableWithC (BaseError ResolutionError) (Eff m)) a
                       -> Evaluator term address value m a
runResolutionErrorWith f = raiseHandler $ runResumableWith (runEvaluator . f)

throwResolutionError :: ( Member (Reader ModuleInfo) sig
                        , Member (Reader Span) sig
                        , Member (Resumable (BaseError ResolutionError)) sig
                        , Carrier sig m
                        )
                     => ResolutionError resume
                     -> Evaluator term address value m resume
throwResolutionError = throwBaseError
