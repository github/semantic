{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields, TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.Ruby.Syntax where

import           Control.Monad (unless)
import qualified Data.Text as T
import           Prologue
import           System.FilePath.Posix

import           Control.Abstract as Abstract hiding (Load, String)
import           Control.Abstract.Heap (Heap, HeapError, insertFrameLink)
import           Control.Abstract.ScopeGraph (insertImportEdge)
import           Control.Abstract.Value (Boolean)
import           Data.Abstract.BaseError
import           Data.Abstract.Evaluatable
import qualified Data.Abstract.Module as M
import           Data.Abstract.Name as Name
import           Data.Abstract.Path
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.JSON.Fields
import qualified Data.Language as Language
import qualified Data.Map.Strict as Map
import qualified Data.Reprinting.Scope as Scope
import qualified Data.Reprinting.Token as Token
import           Data.Semigroup.App
import           Data.Semigroup.Foldable
import           Diffing.Algorithm
import           Proto3.Suite.Class
import           Reprinting.Tokenize hiding (Superclass)

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
  deriving (Eq1, Show1, Ord1) via Generically Send

instance Evaluatable Send where
  eval eval _ Send{..} = do
    sel <- case sendSelector of
             Just sel -> maybeM (throwNoNameError sel) (declaredName sel)
             Nothing  ->
               pure (Name.name "call")

    let self = deref =<< lookupSlot (Declaration __self)
    lhsValue <- maybe self eval sendReceiver
    lhsFrame <- Abstract.scopedEnvironment lhsValue

    let callFunction = do
          reference (Reference sel) (Declaration sel)
          func <- deref =<< lookupSlot (Declaration sel)
          args <- traverse eval sendArgs
          boundFunc <- bindThis lhsValue func
          call boundFunc args -- TODO pass through sendBlock
    maybe callFunction (`withScopeAndFrame` callFunction) lhsFrame

instance Tokenize Send where
  tokenize Send{..} = within Scope.Call $ do
    maybe (pure ()) (\r -> r *> yield Sep) sendReceiver
    fromMaybe (pure ()) sendSelector
    within' Scope.Params $ sequenceA_ (sep sendArgs)
    fromMaybe (pure ()) sendBlock

data Require a = Require { requireRelative :: Bool, requirePath :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Require

instance Evaluatable Require where
  eval eval _ (Require _ x) = do
    name <- eval x >>= asString
    path <- resolveRubyName name
    traceResolve name path
    ((moduleScope, moduleFrame), v) <- doRequire path
    insertImportEdge moduleScope
    insertFrameLink ScopeGraph.Import (Map.singleton moduleScope moduleFrame)
    pure v -- Returns True if the file was loaded, False if it was already loaded. http://ruby-doc.org/core-2.5.0/Kernel.html#method-i-require

instance Tokenize Require where
  tokenize Require{..} = do
    yield . Run $ if requireRelative
                     then "require_relative"
                     else "require"
    within' Scope.Params requirePath

doRequire :: ( Member (Boolean value) sig
             , Member (Modules address value) sig
             , Carrier sig m
             )
          => M.ModulePath
          -> Evaluator term address value m ((address, address), value)
doRequire path = do
  result <- lookupModule path
  case result of
    Nothing                 -> (,) . fst <$> load path <*> boolean True
    Just (scopeAndFrame, _) -> (scopeAndFrame, ) <$> boolean False


data Load a = Load { loadPath :: a, loadWrap :: Maybe a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Load

instance Tokenize Load where
  tokenize Load{..} = do
    yield (Run "load")
    within' Scope.Params $ loadPath *> fromMaybe (pure ()) loadWrap

instance Evaluatable Load where
  eval eval _ (Load x Nothing) = do
    path <- eval x >>= asString
    doLoad path False
  eval eval _ (Load x (Just wrap)) = do
    path <- eval x >>= asString
    shouldWrap <- eval wrap >>= asBool
    doLoad path shouldWrap

doLoad :: ( Member (Boolean value) sig
          , Member (Modules address value) sig
          , Member (Reader (CurrentFrame address)) sig
          , Member (Reader (CurrentScope address)) sig
          , Member (Reader ModuleInfo) sig
          , Member (Reader Span) sig
          , Member (Resumable (BaseError ResolutionError)) sig
          , Member (State (ScopeGraph.ScopeGraph address)) sig
          , Member (State (Heap address address value)) sig
          , Member (Resumable (BaseError (HeapError address))) sig
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
  (moduleScope, moduleFrame) <- fst <$> load path'
  unless shouldWrap $ do
    insertImportEdge moduleScope
    insertFrameLink ScopeGraph.Import (Map.singleton moduleScope moduleFrame)
  boolean Prelude.True -- load always returns true. http://ruby-doc.org/core-2.5.0/Kernel.html#method-i-load

-- TODO: autoload

data Class a = Class { classIdentifier :: !a, classSuperClass :: !(Maybe a), classBody :: !a }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, FreeVariables1, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Class

instance Diffable Class where
  equivalentBySubterm = Just . classIdentifier

instance Evaluatable Class where
  eval eval _ Class{..} = do
    name <- maybeM (throwNoNameError classIdentifier) (declaredName classIdentifier)
    span <- ask @Span
    currentScope' <- currentScope

    let declaration = Declaration name
    maybeSlot <- maybeLookupDeclaration declaration

    case maybeSlot of
      Just slot -> do
        classVal <- deref slot
        maybeFrame <- scopedEnvironment classVal
        case maybeFrame of
          Just classFrame -> withScopeAndFrame classFrame (eval classBody)
          Nothing         -> throwEvalError (DerefError classVal)
      Nothing -> do
        let classSuperclasses = maybeToList classSuperClass
        superScopes <- for classSuperclasses $ \superclass -> do
          name <- maybeM (throwNoNameError superclass) (declaredName superclass)
          scope <- associatedScope (Declaration name)
          slot <- lookupSlot (Declaration name)
          superclassFrame <- scopedEnvironment =<< deref slot
          pure $ case (scope, superclassFrame) of
            (Just scope, Just frame) -> Just (scope, frame)
            _                        -> Nothing

        let superclassEdges = (Superclass, ) . pure . fst <$> catMaybes superScopes
            current = (Lexical, ) <$> pure (pure currentScope')
            edges = Map.fromList (superclassEdges <> current)
        classScope <- newScope edges
        declare (Declaration name) Default Public span (Just classScope)

        let frameEdges = Map.singleton Superclass (Map.fromList (catMaybes superScopes))
        childFrame <- newFrame classScope frameEdges

        withScopeAndFrame childFrame $ do
          void $ eval classBody

        classSlot <- lookupSlot (Declaration name)
        assign classSlot =<< klass (Declaration name) childFrame

        unit

instance Declarations1 Class where
  liftDeclaredName declaredName = declaredName . classIdentifier

instance Tokenize Class where
  tokenize Class{..} = within' Scope.Class $ do
    classIdentifier
    case classSuperClass of
      Just a  -> yield Token.Extends *> a
      Nothing -> pure ()
    classBody


data Module a = Module { moduleIdentifier :: !a, moduleStatements :: ![a] }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Module

instance Evaluatable Module where
  eval eval _ Module{..} =  do
    name <- maybeM (throwNoNameError moduleIdentifier) (declaredName moduleIdentifier)
    span <- ask @Span
    currentScope' <- currentScope

    let declaration = Declaration name
        moduleBody = maybe unit (runApp . foldMap1 (App . eval)) (nonEmpty moduleStatements)
    maybeSlot <- maybeLookupDeclaration declaration

    case maybeSlot of
      Just slot -> do
        moduleVal <- deref slot
        maybeFrame <- scopedEnvironment moduleVal
        case maybeFrame of
          Just moduleFrame -> do
            withScopeAndFrame moduleFrame moduleBody
          Nothing -> throwEvalError (DerefError moduleVal)
      Nothing -> do
        let edges = Map.singleton Lexical [ currentScope' ]
        classScope <- newScope edges
        declare (Declaration name) Default Public span (Just classScope)

        currentFrame' <- currentFrame
        let frameEdges = Map.singleton Lexical (Map.singleton currentScope' currentFrame')
        childFrame <- newFrame classScope frameEdges

        withScopeAndFrame childFrame (void moduleBody)

        moduleSlot <- lookupSlot (Declaration name)
        assign moduleSlot =<< klass (Declaration name) childFrame

        unit

instance Declarations1 Module where
  liftDeclaredName declaredName = declaredName . moduleIdentifier

instance Tokenize Module where
  tokenize Module{..} = do
    yield (Run "module")
    moduleIdentifier
    within' Scope.Namespace $ sequenceA_ moduleStatements


data LowPrecedenceAnd a = LowPrecedenceAnd { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically LowPrecedenceAnd

instance Evaluatable LowPrecedenceAnd where
  -- N.B. we have to use Monad rather than Applicative/Traversable on 'And' and 'Or' so that we don't evaluate both operands
  eval eval _ t = go (fmap eval t) where
    go (LowPrecedenceAnd a b) = do
      cond <- a
      ifthenelse cond b (pure cond)

-- TODO: These should probably be expressed with a new context/token,
-- rather than a literal run, and need to take surrounding precedence
-- into account.
instance Tokenize LowPrecedenceAnd where
  tokenize LowPrecedenceAnd{..} = lhs *> yield (Token.Run "and") <* rhs

data LowPrecedenceOr a = LowPrecedenceOr { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically LowPrecedenceOr

instance Evaluatable LowPrecedenceOr where
  -- N.B. we have to use Monad rather than Applicative/Traversable on 'And' and 'Or' so that we don't evaluate both operands
  eval eval _ t = go (fmap eval t) where
    go (LowPrecedenceOr a b) = do
      cond <- a
      ifthenelse cond (pure cond) b

instance Tokenize LowPrecedenceOr where
  tokenize LowPrecedenceOr{..} = lhs *> yield (Token.Run "or") <* rhs

data Assignment a = Assignment { assignmentContext :: ![a], assignmentTarget :: !a, assignmentValue :: !a }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Assignment

instance Declarations1 Assignment where
  liftDeclaredName declaredName Assignment{..} = declaredName assignmentTarget

instance Evaluatable Assignment where
  eval eval ref Assignment{..} = do
    lhsName <- maybeM (throwNoNameError assignmentTarget) (declaredName assignmentTarget)
    maybeSlot <- maybeLookupDeclaration (Declaration lhsName)
    assignmentSpan <- ask @Span
    maybe (declare (Declaration lhsName) Default Public assignmentSpan Nothing) (const (pure ())) maybeSlot

    lhs <- ref assignmentTarget
    rhs <- eval assignmentValue

    case declaredName assignmentValue of
      Just rhsName -> do
        assocScope <- associatedScope (Declaration rhsName)
        case assocScope of
          Just assocScope' -> do
            objectScope <- newScope (Map.singleton Import [ assocScope' ])
            putSlotDeclarationScope lhs (Just objectScope) -- TODO: not sure if this is right
          Nothing ->
            pure ()
      Nothing ->
        pure ()
    assign lhs rhs
    pure rhs

instance Tokenize Assignment where
  -- Should we be using 'assignmentContext' in here?
  tokenize Assignment{..} = assignmentTarget *> yield Token.Assign <* assignmentValue

-- | A call to @super@ without parentheses in Ruby is known as "zsuper", which has
-- the semantics of invoking @super()@ but implicitly passing the current function's
-- arguments to the @super()@ invocation.
data ZSuper a = ZSuper
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ZSuper

instance Evaluatable ZSuper

instance Tokenize ZSuper where
  tokenize _ = yield $ Run "super"
