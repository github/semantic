{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses #-}
module Language.Python.Syntax where

import           Data.Abstract.Environment as Env
import           Data.Abstract.Evaluatable hiding (friendlyName)
import qualified Data.Abstract.Module as M
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

type QualifiedModuleName = NonEmpty FilePath

moduleName :: ByteString -> QualifiedModuleName
moduleName x = BC.unpack x :| []

qualifiedModuleName :: [ByteString] -> QualifiedModuleName
qualifiedModuleName xs = NonEmpty.fromList (BC.unpack <$> xs)

toName :: QualifiedModuleName -> Name
toName = fmap BC.pack

friendlyName :: QualifiedModuleName -> String
friendlyName xs = intercalate "." (NonEmpty.toList xs)

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
resolvePythonModules :: MonadEvaluatable term value m => QualifiedModuleName -> m (NonEmpty M.ModuleName)
resolvePythonModules qualifiedName = do
  M.Module{..} <- currentModule
  let relRootDir = takeDirectory (makeRelative moduleRoot modulePath)
  for (moduleNames qualifiedName) $ \name -> do
    go relRootDir name
  where
    moduleNames = NonEmpty.scanl1 (</>)
    notFound xs = "Unable to resolve module import: " <> friendlyName qualifiedName <> ", searched: " <> show xs
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
data Import a = Import { importFrom :: !QualifiedModuleName, importSymbols :: ![(Name, Name)], importWildcardToken :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Import where liftEq = genericLiftEq
instance Ord1 Import where liftCompare = genericLiftCompare
instance Show1 Import where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Import where
  eval (Import name xs _) = do
    modulePaths <- resolvePythonModules name
    for_ modulePaths $ \x -> do
      (importedEnv, _) <- isolate (require x)
      modifyEnv (mappend (renamed importedEnv))
    unit
    where
      renamed importedEnv
        | Prologue.null xs = importedEnv
        | otherwise = Env.overwrite xs importedEnv


-- | Qualified Import declarations (symbols are qualified in calling environment).
--
-- If the list of symbols is empty copy and qualify everything to the calling environment.
data QualifiedImport a = QualifiedImport { qualifiedImportFrom :: !QualifiedModuleName, qualifiedImportAlias :: !a, qualifiedImportSymbols :: ![(Name, Name)]}
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 QualifiedImport where liftEq = genericLiftEq
instance Ord1 QualifiedImport where liftCompare = genericLiftCompare
instance Show1 QualifiedImport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QualifiedImport where
  eval (QualifiedImport name alias xs) = do
    modulePaths <- resolvePythonModules name
    for_ modulePaths $ \x -> do
      (importedEnv, _) <- isolate (require x)
      modifyEnv (mappend (Env.overwrite (renames importedEnv) importedEnv))
    unit
    where
      renames importedEnv
        | Prologue.null xs = fmap prepend (Env.names importedEnv)
        | otherwise = xs
      prefix = freeVariable (subterm alias)
      prepend n = (n, prefix <> n)

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
