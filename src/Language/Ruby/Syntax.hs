{-# LANGUAGE DeriveAnyClass #-}
module Language.Ruby.Syntax where

import Control.Monad (unless)
import Data.Abstract.Evaluatable
import Data.Abstract.Path hiding (dropExtension)
import Data.Abstract.ModuleTable
import qualified Data.Abstract.Module as M
import Diffing.Algorithm
import Prelude hiding (fail)
import Prologue
import qualified Data.ByteString.Char8 as BC
import System.FilePath.Posix


-- TODO: Fully sort out ruby require/load mechanics
--
-- require "json"
resolveRubyName :: MonadEvaluatable term value m => ByteString -> m M.ModuleName
resolveRubyName n = resolve [name <.> "rb"] >>= maybeFail notFound
  where name = toName n
        notFound = "Unable to resolve: " <> name
        toName = BC.unpack . dropRelativePrefix . stripQuotes

-- load "/root/src/file.rb"
resolveRubyPath :: MonadEvaluatable term value m => ByteString -> m M.ModuleName
resolveRubyPath n = resolve [name] >>= maybeFail notFound
  where name = toName n
        notFound = "Unable to resolve: " <> name
        toName = BC.unpack . dropRelativePrefix . stripQuotes

data Require a = Require { requireRelative :: Bool, requirePath :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Require where liftEq = genericLiftEq
instance Ord1 Require where liftCompare = genericLiftCompare
instance Show1 Require where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Require where
  eval (Require _ x) = do
    name <- subtermValue x >>= asString
    path <- resolveRubyName name
    (importedEnv, v) <- isolate (doRequire path)
    modifyEnv (mappend importedEnv)
    pure v -- Returns True if the file was loaded, False if it was already loaded. http://ruby-doc.org/core-2.5.0/Kernel.html#method-i-require

doRequire :: MonadEvaluatable term value m
          => ModuleName
          -> m (EnvironmentFor value, value)
doRequire name = do
  moduleTable <- getModuleTable
  case moduleTableLookup name moduleTable of
    Nothing -> (,) <$> (fst <$> load name) <*> boolean True
    Just (env, _) -> (,) <$> pure env <*> boolean False


newtype Load a = Load { loadArgs :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Load where liftEq = genericLiftEq
instance Ord1 Load where liftCompare = genericLiftCompare
instance Show1 Load where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Load where
  eval (Load [x]) = do
    path <- subtermValue x >>= asString
    doLoad path False
  eval (Load [x, wrap]) = do
    path <- subtermValue x >>= asString
    shouldWrap <- subtermValue wrap >>= toBool
    doLoad path shouldWrap
  eval (Load _) = fail "invalid argument supplied to load, path is required"

doLoad :: MonadEvaluatable term value m => ByteString -> Bool -> m value
doLoad path shouldWrap = do
  path' <- resolveRubyPath path
  (importedEnv, _) <- isolate (load path')
  unless shouldWrap $ modifyEnv (mappend importedEnv)
  boolean Prelude.True -- load always returns true. http://ruby-doc.org/core-2.5.0/Kernel.html#method-i-load

-- TODO: autoload

data Class a = Class { classIdentifier :: !a, classSuperClasses :: ![a], classBody :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Diffable Class where
  equivalentBySubterm = Just . classIdentifier

instance Eq1 Class where liftEq = genericLiftEq
instance Ord1 Class where liftCompare = genericLiftCompare
instance Show1 Class where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Class where
  eval Class{..} = do
    supers <- traverse subtermValue classSuperClasses
    letrec' name $ \addr ->
      subtermValue classBody <* makeNamespace name addr supers
    where name = freeVariable (subterm classIdentifier)

data Module a = Module { moduleIdentifier :: !a, moduleStatements :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Module where
  eval (Module iden xs) = letrec' name $ \addr ->
    eval xs <* makeNamespace name addr []
    where name = freeVariable (subterm iden)
