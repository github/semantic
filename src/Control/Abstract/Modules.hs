{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
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
import Data.Abstract.BaseError
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Language
import qualified Data.Set as Set
import Prologue
import Source.Span
import System.FilePath.Posix (takeDirectory)

-- | A scope address, frame address, and value ref.
--
-- Partially applied, omitting the value type for type applications at * -> *.
type ModuleResult address = (,) (address, address)

-- | Retrieve an evaluated module, if any. @Nothing@ means we’ve never tried to load it, and @Just (env, value)@ indicates the result of a completed load.
lookupModule :: (Member (Modules address value) sig, Carrier sig m) => ModulePath -> Evaluator term address value m (Maybe (ModuleResult address value))
lookupModule = sendModules . flip Lookup pure

-- | Resolve a list of module paths to a possible module table entry.
resolve :: (Member (Modules address value) sig, Carrier sig m) => [FilePath] -> Evaluator term address value m (Maybe ModulePath)
resolve = sendModules . flip Resolve pure

listModulesInDir :: (Member (Modules address value) sig, Carrier sig m) => FilePath -> Evaluator term address value m [ModulePath]
listModulesInDir = sendModules . flip List pure


-- | Require/import another module by name and return its environment and value.
--
-- Looks up the module's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: (Member (Modules address value) sig, Carrier sig m) => ModulePath -> Evaluator term address value m (ModuleResult address value)
require path = lookupModule path >>= maybeM (load path)

-- | Load another module by name and return its environment and value.
--
-- Always loads/evaluates.
load :: (Member (Modules address value) sig, Carrier sig m) => ModulePath -> Evaluator term address value m (ModuleResult address value)
load path = sendModules (Load path pure)


data Modules address value (m :: * -> *) k
  = Load    ModulePath (ModuleResult address value -> m k)
  | Lookup  ModulePath (Maybe (ModuleResult address value) -> m k)
  | Resolve [FilePath] (Maybe ModulePath -> m k)
  | List    FilePath   ([ModulePath] -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Modules address value)
instance Effect   (Modules address value)


sendModules :: ( Member (Modules address value) sig
               , Carrier sig m)
            => Modules address value (Evaluator term address value m) return
            -> Evaluator term address value m return
sendModules = send

runModules :: Set ModulePath
           -> Evaluator term address value (ModulesC address value m) a
           -> Evaluator term address value m a
runModules paths = raiseHandler (runReader paths . runModulesC)

newtype ModulesC address value m a = ModulesC { runModulesC :: ReaderC (Set ModulePath) m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadIO)

instance ( Member (Reader (ModuleTable (Module (ModuleResult address value)))) sig
         , Member (Resumable (BaseError (LoadError address value))) sig
         , Carrier sig m
         )
      => Carrier (Modules address value :+: sig) (ModulesC address value m) where
  eff (L op) = do
    paths <- ModulesC ask
    case op of
      Load    name  k -> askModuleTable >>= maybeM (throwLoadError (ModuleNotFoundError name)) . fmap moduleBody . ModuleTable.lookup name >>= k
      Lookup  path  k -> askModuleTable >>= k . fmap moduleBody . ModuleTable.lookup path
      Resolve names k -> k (find (`Set.member` paths) names)
      List    dir   k -> k (filter ((dir ==) . takeDirectory) (toList paths))
  eff (R other) = ModulesC (eff (R (handleCoercible other)))

askModuleTable :: (Member (Reader (ModuleTable (Module (ModuleResult address value)))) sig, Carrier sig m) => m (ModuleTable (Module (ModuleResult address value)))
askModuleTable = ask


-- | An error thrown when loading a module from the list of provided modules. Indicates we weren't able to find a module with the given name.
data LoadError address value resume where
  ModuleNotFoundError :: ModulePath -> LoadError address value (ModuleResult address value)

deriving instance Eq (LoadError address value resume)
deriving instance Show (LoadError address value resume)
instance Show1 (LoadError address value) where
  liftShowsPrec _ _ = showsPrec
instance Eq1 (LoadError address value) where
  liftEq _ (ModuleNotFoundError a) (ModuleNotFoundError b) = a == b

runLoadError :: Evaluator term address value (ResumableC (BaseError (LoadError address value)) m) a
             -> Evaluator term address value m (Either (SomeError (BaseError (LoadError address value))) a)
runLoadError = raiseHandler runResumable

runLoadErrorWith :: (forall resume . (BaseError (LoadError address value)) resume -> Evaluator term address value m resume)
                 -> Evaluator term address value (ResumableWithC (BaseError (LoadError address value)) m) a
                 -> Evaluator term address value m a
runLoadErrorWith f = raiseHandler $ runResumableWith (runEvaluator . f)

throwLoadError :: (Member (Resumable (BaseError (LoadError address value))) sig, Carrier sig m)
               => LoadError address value resume
               -> m resume
throwLoadError err@(ModuleNotFoundError name) = throwResumable $ BaseError (ModuleInfo name Unknown mempty) lowerBound err
-- TODO: Might be able to get rest of ModuleInfo from the env ^.


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

runResolutionError :: Evaluator term address value (ResumableC (BaseError ResolutionError) m) a
                   -> Evaluator term address value m (Either (SomeError (BaseError ResolutionError)) a)
runResolutionError = raiseHandler runResumable

runResolutionErrorWith :: (forall resume . (BaseError ResolutionError) resume -> Evaluator term address value m resume)
                       -> Evaluator term address value (ResumableWithC (BaseError ResolutionError) m) a
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
