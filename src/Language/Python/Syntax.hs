{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses #-}
module Language.Python.Syntax where

import           Data.Abstract.Environment as Env
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import           Data.Align.Generic
import qualified Data.ByteString.Char8 as BC
import           Data.Functor.Classes.Generic
import           Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Mergeable
import           Diffing.Algorithm
import           GHC.Generics
import           Prologue
import           Prelude hiding (fail)
import           System.FilePath.Posix

data QualifiedModuleName = QualifiedModuleName { unQualifiedModuleName :: NonEmpty FilePath }
                         | RelativeQualifiedModuleName { relativePrefix :: FilePath, m :: Maybe QualifiedModuleName }

  deriving (Eq, Ord, Show)

qualifiedModuleName :: NonEmpty ByteString -> QualifiedModuleName
qualifiedModuleName xs = QualifiedModuleName (BC.unpack <$> xs)

relativeModuleName :: ByteString -> [ByteString] -> QualifiedModuleName
relativeModuleName prefix [] = RelativeQualifiedModuleName (BC.unpack prefix) Nothing
relativeModuleName prefix paths = RelativeQualifiedModuleName (BC.unpack prefix) (Just (qualifiedModuleName (NonEmpty.fromList paths)))

friendlyName :: QualifiedModuleName -> String
friendlyName (QualifiedModuleName xs) = intercalate "." (NonEmpty.toList xs)
friendlyName (RelativeQualifiedModuleName prefix qn) = prefix <> maybe "" friendlyName qn

-- Python module resolution.
-- https://docs.python.org/3/reference/import.html#importsystem
--
-- TODO: Namespace packages
--
-- Regular packages resolution:
--
-- parent/
--     __init__.py
--     one/
--         __init__.py
--     two/
--         __init__.py
--     three/
--         __init__.py
--
-- `import parent.one` will implicitly execute:
--     `parent/__init__.py` and
--     `parent/one/__init__.py`
-- Subsequent imports of `parent.two` or `parent.three` will execute
--     `parent/two/__init__.py` and
--     `parent/three/__init__.py` respectively.
resolvePythonModules :: MonadEvaluatable location term value m => QualifiedModuleName -> m (NonEmpty ModulePath)
resolvePythonModules q = do
  ModuleInfo{..} <- currentModule
  let relRootDir = takeDirectory (makeRelative moduleRoot modulePath)
  for (moduleNames q) $ \name -> do
    x <- trace ("resolving: " <> show name) $ go relRootDir name
    trace ("found: " <> show x) (pure x)
  where
    -- TODO: deal with relative .. and ..., etc imports later
    moduleNames :: QualifiedModuleName -> NonEmpty FilePath
    moduleNames QualifiedModuleName{..} = NonEmpty.scanl1 (</>) unQualifiedModuleName
    moduleNames (RelativeQualifiedModuleName x Nothing) = error $ "importing from '" <> show x <> "' is not implemented"
    moduleNames (RelativeQualifiedModuleName "." (Just paths)) = moduleNames paths
    moduleNames (RelativeQualifiedModuleName x (Just paths)) = error $ "importing from '" <> show x <> "' is not implemented " <> show paths

    notFound xs = "Unable to resolve module import: " <> friendlyName q <> ", searched: " <> show xs
    go rootDir x = do
      let path = normalise (rootDir </> normalise x)
      let searchPaths = [ path </> "__init__.py"
                        , path <.> ".py"
                        ]
      trace ("searching in: " <> show searchPaths) $
        resolve searchPaths >>= maybeFail (notFound searchPaths)


-- | Import declarations (symbols are added directly to the calling environment).
--
-- If the list of symbols is empty copy everything to the calling environment.
data Import a = Import { importFrom :: QualifiedModuleName, importSymbols :: ![(Name, Name)] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Import where liftEq = genericLiftEq
instance Ord1 Import where liftCompare = genericLiftCompare
instance Show1 Import where liftShowsPrec = genericLiftShowsPrec

-- from a import b
-- from a import b as c
-- from a import *
instance Evaluatable Import where
  eval (Import name xs) = do
    modulePaths <- resolvePythonModules name

    -- Eval parent modules first
    for_ (NonEmpty.init modulePaths) (isolate . require)

    -- Last module path is the one we want to import
    let path = NonEmpty.last modulePaths
    (importedEnv, _) <- isolate (require path)
    modifyEnv (mappend (select importedEnv))
    unit
    where
      select importedEnv
        | Prologue.null xs = importedEnv
        | otherwise = Env.overwrite xs importedEnv


newtype QualifiedImport a = QualifiedImport { qualifiedImportFrom :: QualifiedModuleName }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 QualifiedImport where liftEq = genericLiftEq
instance Ord1 QualifiedImport where liftCompare = genericLiftCompare
instance Show1 QualifiedImport where liftShowsPrec = genericLiftShowsPrec

-- import a.b.c
instance Evaluatable QualifiedImport where
  eval (QualifiedImport name@QualifiedModuleName{..}) = do
    modulePaths <- resolvePythonModules name
    go (NonEmpty.zip (BC.pack <$> unQualifiedModuleName) modulePaths)
    where
      -- Evaluate and import the last module, updating the environment
      go ((name, path) :| []) = letrec' name $ \addr -> do
        (importedEnv, _) <- isolate (require path)
        modifyEnv (mappend importedEnv)
        void $ makeNamespace name addr []
        unit
      -- Evaluate each parent module, creating a just namespace
      go ((name, path) :| xs) = letrec' name $ \addr -> do
        void $ isolate (require path)
        void $ go (NonEmpty.fromList xs)
        makeNamespace name addr []
  eval (QualifiedImport _) = fail "technically this is not allowed in python"

data QualifiedAliasedImport a = QualifiedAliasedImport { qualifiedAliasedImportFrom :: QualifiedModuleName, qualifiedAliasedImportAlias :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 QualifiedAliasedImport where liftEq = genericLiftEq
instance Ord1 QualifiedAliasedImport where liftCompare = genericLiftCompare
instance Show1 QualifiedAliasedImport where liftShowsPrec = genericLiftShowsPrec

-- import a.b.c as e
instance Evaluatable QualifiedAliasedImport where
  eval (QualifiedAliasedImport name aliasTerm) = do
    modulePaths <- resolvePythonModules name

    -- Evaluate each parent module
    for_ (NonEmpty.init modulePaths) (isolate . require)

    -- Evaluate and import the last module, aliasing and updating the environment
    let alias = freeVariable (subterm aliasTerm)
    letrec' alias $ \addr -> do
      let path = NonEmpty.last modulePaths
      (importedEnv, _) <- isolate (require path)
      modifyEnv (mappend importedEnv)
      void $ makeNamespace alias addr []
      unit

-- | Ellipsis (used in splice expressions and alternatively can be used as a fill in expression, like `undefined` in Haskell)
data Ellipsis a = Ellipsis
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Ellipsis where liftEq = genericLiftEq
instance Ord1 Ellipsis where liftCompare = genericLiftCompare
instance Show1 Ellipsis where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Ellipsis
instance Evaluatable Ellipsis


data Redirect a = Redirect !a !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Redirect where liftEq = genericLiftEq
instance Ord1 Redirect where liftCompare = genericLiftCompare
instance Show1 Redirect where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Redirect
instance Evaluatable Redirect
