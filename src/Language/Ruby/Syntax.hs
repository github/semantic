{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields, TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.Ruby.Syntax where

import           Control.Monad (unless)
import qualified Data.Text as T
import           Prologue
import           System.FilePath.Posix

import           Control.Abstract.Value (Boolean)
import           Data.Abstract.BaseError
import           Data.Abstract.Evaluatable
import qualified Data.Abstract.Module as M
import           Data.Abstract.Path
import qualified Data.Reprinting.Scope as Scope
import           Data.JSON.Fields
import qualified Data.Language as Language
import           Diffing.Algorithm
import           Proto3.Suite.Class
import           Reprinting.Tokenize
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import Control.Abstract.ScopeGraph (bindAll, insertImportEdge, ScopeError)

-- TODO: Fully sort out ruby require/load mechanics
--
-- require "json"
resolveRubyName :: ( Member (Modules address value) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (Reader Span) sig
                   , Member (Resumable (BaseError ResolutionError)) sig
                   , Carrier sig m
                   )
                => Text
                -> Evaluator term address value m M.ModulePath
resolveRubyName name = do
  let name' = cleanNameOrPath name
  let paths = [name' <.> "rb"]
  modulePath <- resolve paths
  maybeM (throwResolutionError $ NotFoundError name' paths Language.Ruby) modulePath

-- load "/root/src/file.rb"
resolveRubyPath :: ( Member (Modules address value) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (Reader Span) sig
                   , Member (Resumable (BaseError ResolutionError)) sig
                   , Carrier sig m
                   )
                => Text
                -> Evaluator term address value m M.ModulePath
resolveRubyPath path = do
  let name' = cleanNameOrPath path
  modulePath <- resolve [name']
  maybeM (throwResolutionError $ NotFoundError name' [name'] Language.Ruby) modulePath

cleanNameOrPath :: Text -> String
cleanNameOrPath = T.unpack . dropRelativePrefix . stripQuotes

data Send a = Send { sendReceiver :: Maybe a, sendSelector :: Maybe a, sendArgs :: [a], sendBlock :: Maybe a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Send where liftEq = genericLiftEq
instance Ord1 Send where liftCompare = genericLiftCompare
instance Show1 Send where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Send where
  eval _ Send{..} = undefined -- do
    -- let sel = case sendSelector of
    --       Just sel -> eval sel >>= address
    --       Nothing  -> variable (name "call")
    -- recv <- maybe (self >>= maybeM (box unit)) (eval >=> address) sendReceiver
    -- func <- deref =<< evaluateInScopedEnv recv sel
    -- args <- traverse (eval >=> address) sendArgs
    -- Rval <$> call func recv args -- TODO pass through sendBlock

instance Tokenize Send where
  tokenize Send{..} = within Scope.Call $ do
    maybe (pure ()) (\r -> r *> yield Sep) sendReceiver
    fromMaybe (pure ()) sendSelector
    within' Scope.Params $ sequenceA_ (sep sendArgs)
    fromMaybe (pure ()) sendBlock

data Require a = Require { requireRelative :: Bool, requirePath :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Require where liftEq = genericLiftEq
instance Ord1 Require where liftCompare = genericLiftCompare
instance Show1 Require where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Require where
  eval eval (Require _ x) = do
    name <- eval x >>= value >>= asString
    path <- resolveRubyName name
    traceResolve name path
    (scopeGraph, v) <- doRequire path
    maybe (pure ()) insertImportEdge (ScopeGraph.currentScope scopeGraph)
    rvalBox v -- Returns True if the file was loaded, False if it was already loaded. http://ruby-doc.org/core-2.5.0/Kernel.html#method-i-require

doRequire :: ( Member (Boolean value) sig
             , Member (Modules address value) sig
             , Carrier sig m
             )
          => M.ModulePath
          -> Evaluator term address value m (ScopeGraph.ScopeGraph address, value)
doRequire path = do
  result <- lookupModule path
  case result of
    Nothing       -> (,) . fst <$> load path <*> boolean True
    Just (scopeGraph, _) -> (scopeGraph,) <$> boolean False


data Load a = Load { loadPath :: a, loadWrap :: Maybe a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Load where liftEq = genericLiftEq
instance Ord1 Load where liftCompare = genericLiftCompare
instance Show1 Load where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Load where
  eval eval (Load x Nothing) = do
    path <- eval x >>= value >>= asString
    rvalBox =<< doLoad path False
  eval eval (Load x (Just wrap)) = do
    path <- eval x >>= value >>= asString
    shouldWrap <- eval wrap >>= value >>= asBool
    rvalBox =<< doLoad path shouldWrap

doLoad :: ( Member (Boolean value) sig
          , Member (Modules address value) sig
          , Member (Reader ModuleInfo) sig
          , Member (Reader Span) sig
          , Member (Resumable (BaseError ResolutionError)) sig
          , Member (Resumable (BaseError (ScopeError address))) sig
          , Member (State (ScopeGraph.ScopeGraph address)) sig
          , Member Trace sig
          , Ord address
          , Carrier sig m
          )
       => Text
       -> Bool
       -> Evaluator term address value m value
doLoad path shouldWrap = do
  path' <- resolveRubyPath path
  traceResolve path path'
  scopeGraph <- fst <$> load path'
  unless shouldWrap $ do
    maybe (pure ()) insertImportEdge (ScopeGraph.currentScope scopeGraph)
  boolean Prelude.True -- load always returns true. http://ruby-doc.org/core-2.5.0/Kernel.html#method-i-load

-- TODO: autoload

data Class a = Class { classIdentifier :: !a, classSuperClass :: !(Maybe a), classBody :: !a }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, FreeVariables1, ToJSONFields1, Named1, Message1, NFData1)

instance Diffable Class where
  equivalentBySubterm = Just . classIdentifier

instance Eq1 Class where liftEq = genericLiftEq
instance Ord1 Class where liftCompare = genericLiftCompare
instance Show1 Class where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Class where
  eval _ Class{..} = undefined -- do
    -- super <- traverse (eval >=> address) classSuperClass
    -- name <- maybeM (throwEvalError NoNameError) (declaredName classIdentifier)
    -- rvalBox =<< letrec' name (\addr ->
    --   makeNamespace name addr super (void (eval classBody)))

instance Declarations1 Class where
  liftDeclaredName declaredName = declaredName . classIdentifier

data Module a = Module { moduleIdentifier :: !a, moduleStatements :: ![a] }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Module where
  eval _ (Module _ _) = undefined -- do
    -- name <- maybeM (throwEvalError NoNameError) (declaredName iden)
    -- rvalBox =<< letrec' name (\addr ->
    --   makeNamespace name addr Nothing (traverse_ eval xs))

instance Declarations1 Module where
  liftDeclaredName declaredName = declaredName . moduleIdentifier

data LowPrecedenceAnd a = LowPrecedenceAnd { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Evaluatable LowPrecedenceAnd where
  -- N.B. we have to use Monad rather than Applicative/Traversable on 'And' and 'Or' so that we don't evaluate both operands
  eval eval t = rvalBox =<< go (fmap (eval >=> value) t) where
    go (LowPrecedenceAnd a b) = do
      cond <- a
      ifthenelse cond b (pure cond)

instance Eq1 LowPrecedenceAnd where liftEq = genericLiftEq
instance Ord1 LowPrecedenceAnd where liftCompare = genericLiftCompare
instance Show1 LowPrecedenceAnd where liftShowsPrec = genericLiftShowsPrec

data LowPrecedenceOr a = LowPrecedenceOr { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Evaluatable LowPrecedenceOr where
  -- N.B. we have to use Monad rather than Applicative/Traversable on 'And' and 'Or' so that we don't evaluate both operands
  eval eval t = rvalBox =<< go (fmap (eval >=> value) t) where
    go (LowPrecedenceOr a b) = do
      cond <- a
      ifthenelse cond (pure cond) b

instance Eq1 LowPrecedenceOr where liftEq = genericLiftEq
instance Ord1 LowPrecedenceOr where liftCompare = genericLiftCompare
instance Show1 LowPrecedenceOr where liftShowsPrec = genericLiftShowsPrec
