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
import           System.FilePath.Posix


-- 1. Regular data type (not parameterized) - easy to work with, loss of diff info
-- 2. Parameterize and use freeVariables to get filepath info out (hacky)
-- 3. Parameterize and use eval (Evaluatable instance) and `subtermValue x >>= asString` in syntaxes like QualifiedImport

-- TODO: Model relative imports
-- import .a
-- import ..a

newtype QualifiedModuleName = QualifiedModuleName { unQualifiedModuleName :: NonEmpty FilePath }
  deriving (Eq, Ord, Show)
  -- deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

-- instance Eq1 QualifiedModuleName where liftEq = genericLiftEq
-- instance Ord1 QualifiedModuleName where liftCompare = genericLiftCompare
-- instance Show1 QualifiedModuleName where liftShowsPrec = genericLiftShowsPrec

moduleName :: ByteString -> QualifiedModuleName
moduleName x = QualifiedModuleName $ BC.unpack x :| []

qualifiedModuleName :: [ByteString] -> QualifiedModuleName
qualifiedModuleName xs = QualifiedModuleName $ NonEmpty.fromList (BC.unpack <$> xs)

friendlyName :: QualifiedModuleName -> String
friendlyName (QualifiedModuleName xs) = intercalate "." (NonEmpty.toList xs)

-- Python module resolution.
--
-- https://docs.python.org/3/reference/import.html#importsystem
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
resolvePythonModules q@(QualifiedModuleName qualifiedName) = do
  ModuleInfo{..} <- currentModule
  let relRootDir = takeDirectory (makeRelative moduleRoot modulePath)
  for (moduleNames qualifiedName) $ \name -> do
    go relRootDir name
  where
    moduleNames = NonEmpty.scanl1 (</>)
    notFound xs = "Unable to resolve module import: " <> friendlyName q <> ", searched: " <> show xs
    go rootDir x = do
      let path = normalise (rootDir </> normalise x)
      let searchPaths = [ path </> "__init__.py"
                        , path <.> ".py"
                        ]
      trace ("searched: " <> show searchPaths) $
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
