{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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

import           Control.Algebra
import           Control.Carrier.Reader
import qualified Control.Carrier.Resumable.Either as Either
import qualified Control.Carrier.Resumable.Resume as With
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Functor.Classes
import           Data.Maybe.Exts
import           Data.Semilattice.Lower
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic1)
import           Source.Span
import           System.FilePath.Posix (takeDirectory)

import Control.Abstract.Evaluator
import Data.Abstract.BaseError
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Language

-- | A scope address, frame address, and value ref.
--
-- Partially applied, omitting the value type for type applications at * -> *.
type ModuleResult address = (,) (address, address)

-- | Retrieve an evaluated module, if any. @Nothing@ means we’ve never tried to load it, and @Just (env, value)@ indicates the result of a completed load.
lookupModule :: Has (Modules address value) sig m => ModulePath -> Evaluator term address value m (Maybe (ModuleResult address value))
lookupModule = sendModules . flip Lookup pure

-- | Resolve a list of module paths to a possible module table entry.
resolve :: Has (Modules address value) sig m => [FilePath] -> Evaluator term address value m (Maybe ModulePath)
resolve = sendModules . flip Resolve pure

listModulesInDir :: Has (Modules address value) sig m => FilePath -> Evaluator term address value m [ModulePath]
listModulesInDir = sendModules . flip List pure


-- | Require/import another module by name and return its environment and value.
--
-- Looks up the module's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: Has (Modules address value) sig m => ModulePath -> Evaluator term address value m (ModuleResult address value)
require path = lookupModule path >>= maybeM (load path)

-- | Load another module by name and return its environment and value.
--
-- Always loads/evaluates.
load :: Has (Modules address value) sig m => ModulePath -> Evaluator term address value m (ModuleResult address value)
load path = sendModules (Load path pure)


data Modules address value (m :: * -> *) k
  = Load    ModulePath (ModuleResult address value -> m k)
  | Lookup  ModulePath (Maybe (ModuleResult address value) -> m k)
  | Resolve [FilePath] (Maybe ModulePath -> m k)
  | List    FilePath   ([ModulePath] -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Modules address value)
instance Effect   (Modules address value)


sendModules :: Has (Modules address value) sig m
            => Modules address value (Evaluator term address value m) return
            -> Evaluator term address value m return
sendModules = send

runModules :: Set ModulePath
           -> Evaluator term address value (ModulesC address value m) a
           -> Evaluator term address value m a
runModules paths = raiseHandler (runReader paths . runModulesC)

newtype ModulesC address value m a = ModulesC { runModulesC :: ReaderC (Set ModulePath) m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadIO)

instance ( Has (Reader (ModuleTable (Module (ModuleResult address value)))) sig m
         , Has (Resumable (BaseError (LoadError address value))) sig m
         )
      => Algebra (Modules address value :+: sig) (ModulesC address value m) where
  alg (L op) = do
    paths <- ModulesC ask
    case op of
      Load    name  k -> askModuleTable >>= maybeM (throwLoadError (ModuleNotFoundError name)) . fmap moduleBody . ModuleTable.lookup name >>= k
      Lookup  path  k -> askModuleTable >>= k . fmap moduleBody . ModuleTable.lookup path
      Resolve names k -> k (find (`Set.member` paths) names)
      List    dir   k -> k (filter ((dir ==) . takeDirectory) (toList paths))
  alg (R other) = ModulesC (alg (R (handleCoercible other)))

askModuleTable :: Has (Reader (ModuleTable (Module (ModuleResult address value)))) sig m => m (ModuleTable (Module (ModuleResult address value)))
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

runLoadError :: Evaluator term address value (Either.ResumableC (BaseError (LoadError address value)) m) a
             -> Evaluator term address value m (Either (Either.SomeError (BaseError (LoadError address value))) a)
runLoadError = raiseHandler Either.runResumable

runLoadErrorWith :: (forall resume . (BaseError (LoadError address value)) resume -> Evaluator term address value m resume)
                 -> Evaluator term address value (With.ResumableC (BaseError (LoadError address value)) m) a
                 -> Evaluator term address value m a
runLoadErrorWith f = raiseHandler $ With.runResumable (runEvaluator . f)

throwLoadError :: Has (Resumable (BaseError (LoadError address value))) sig m
               => LoadError address value resume
               -> m resume
throwLoadError err@(ModuleNotFoundError name) = throwResumable $ BaseError (ModuleInfo name "Unknown" mempty) lowerBound err
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
  liftEq _ (GoImportError a) (GoImportError b)           = a == b
  liftEq _ _ _                                           = False

runResolutionError :: Evaluator term address value (Either.ResumableC (BaseError ResolutionError) m) a
                   -> Evaluator term address value m (Either (Either.SomeError (BaseError ResolutionError)) a)
runResolutionError = raiseHandler Either.runResumable

runResolutionErrorWith :: (forall resume . (BaseError ResolutionError) resume -> Evaluator term address value m resume)
                       -> Evaluator term address value (With.ResumableC (BaseError ResolutionError) m) a
                       -> Evaluator term address value m a
runResolutionErrorWith f = raiseHandler $ With.runResumable (runEvaluator . f)

throwResolutionError :: ( Has (Reader ModuleInfo) sig m
                        , Has (Reader Span) sig m
                        , Has (Resumable (BaseError ResolutionError)) sig m
                        )
                     => ResolutionError resume
                     -> Evaluator term address value m resume
throwResolutionError = throwBaseError
