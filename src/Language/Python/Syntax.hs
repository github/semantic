{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses #-}
module Language.Python.Syntax where

import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable
import Data.Abstract.Path
import Data.Align.Generic
import Data.Functor.Classes.Generic
import Data.Mergeable
import Diffing.Algorithm
import GHC.Generics
import Prologue

-- | Import declarations (symbols are added directly to the calling environment).
--
-- If the list of symbols is empty copy everything to the calling environment.
data Import a = Import { importFrom :: Path, importSymbols :: ![(Name, Name)], importWildcardToken :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Import where liftEq = genericLiftEq
instance Ord1 Import where liftCompare = genericLiftCompare
instance Show1 Import where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Import where
  eval (Import (Path name _) xs _) = do
    (importedEnv, _) <- isolate (require name)
    modifyEnv (mappend (renamed importedEnv))
    unit
    where
      renamed importedEnv
        | Prologue.null xs = importedEnv
        | otherwise = Env.overwrite xs importedEnv


-- | Qualified Import declarations (symbols are qualified in calling environment).
--
-- If the list of symbols is empty copy and qualify everything to the calling environment.
data QualifiedImport a = QualifiedImport { qualifiedImportFrom :: !Path, qualifiedImportAlias :: !a, qualifiedImportSymbols :: ![(Name, Name)]}
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 QualifiedImport where liftEq = genericLiftEq
instance Ord1 QualifiedImport where liftCompare = genericLiftCompare
instance Show1 QualifiedImport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QualifiedImport where
  eval (QualifiedImport (Path name _) alias xs) = do
    (importedEnv, _) <- isolate (require name)
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
