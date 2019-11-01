{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, FlexibleContexts, OverloadedStrings, RecordWildCards, TupleSections, TypeApplications #-}
module Language.Ruby.Syntax (module Language.Ruby.Syntax) where

import Prologue

import           Control.Abstract as Abstract hiding (Load, String)
import           Control.Abstract.Heap (Heap, HeapError, insertFrameLink)
import           Control.Abstract.ScopeGraph (insertImportEdge)
import           Control.Abstract.Value (Boolean)
import           Control.Monad (unless)
import           Data.Abstract.BaseError
import           Data.Abstract.Evaluatable
import qualified Data.Abstract.Module as M
import           Data.Abstract.Name as Name
import           Data.Abstract.Path
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.JSON.Fields
import qualified Data.Language as Language
import qualified Data.Map.Strict as Map
import           Data.Semigroup.App
import           Data.Semigroup.Foldable
import qualified Data.Text as T
import           Diffing.Algorithm
import           System.FilePath.Posix

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
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Send where liftEq = genericLiftEq
instance Ord1 Send where liftCompare = genericLiftCompare
instance Show1 Send where liftShowsPrec = genericLiftShowsPrec

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
          span <- ask @Span
          reference (Reference sel) span ScopeGraph.Call (Declaration sel)
          func <- deref =<< lookupSlot (Declaration sel)
          args <- traverse eval sendArgs
          boundFunc <- bindThis lhsValue func
          call boundFunc args -- TODO pass through sendBlock
    maybe callFunction (`withScopeAndFrame` callFunction) lhsFrame

data Require a = Require { requireRelative :: Bool, requirePath :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Require where liftEq = genericLiftEq
instance Ord1 Require where liftCompare = genericLiftCompare
instance Show1 Require where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Require where
  eval eval _ (Require _ x) = do
    name <- eval x >>= asString
    path <- resolveRubyName name
    traceResolve name path
    ((moduleScope, moduleFrame), v) <- doRequire path
    insertImportEdge moduleScope
    insertFrameLink ScopeGraph.Import (Map.singleton moduleScope moduleFrame)
    pure v -- Returns True if the file was loaded, False if it was already loaded. http://ruby-doc.org/core-2.5.0/Kernel.html#method-i-require

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
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Load where liftEq = genericLiftEq
instance Ord1 Load where liftCompare = genericLiftCompare
instance Show1 Load where liftShowsPrec = genericLiftShowsPrec

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
  deriving (Foldable, Traversable, Functor, Generic1, Hashable1, FreeVariables1, ToJSONFields1)

instance Eq1 Class where liftEq = genericLiftEq
instance Ord1 Class where liftCompare = genericLiftCompare
instance Show1 Class where liftShowsPrec = genericLiftShowsPrec

instance Diffable Class where
  equivalentBySubterm = Just . classIdentifier

instance Evaluatable Class where
  eval eval _ Class{..} = do
    (name, relation) <- case declaredName classIdentifier of
                          Just name -> pure (name, Default)
                          _         -> gensym >>= \name -> pure (name, Gensym)
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
        declare (Declaration name) relation Public span ScopeGraph.Class (Just classScope)

        let frameEdges = Map.singleton Superclass (Map.fromList (catMaybes superScopes))
        childFrame <- newFrame classScope frameEdges

        withScopeAndFrame childFrame $ do
          void $ eval classBody

        classSlot <- lookupSlot (Declaration name)
        assign classSlot =<< klass (Declaration name) childFrame

        unit

instance Declarations1 Class where
  liftDeclaredName declaredName = declaredName . classIdentifier


data Module a = Module { moduleIdentifier :: !a, moduleStatements :: ![a] }
  deriving (Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Module where
  eval eval _ Module{..} =  do
    (name, relation) <- case declaredName moduleIdentifier of
                          Just name -> pure (name, Default)
                          _         -> gensym >>= \name -> pure (name, Gensym)
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
        declare (Declaration name) relation Public span ScopeGraph.Module (Just classScope)

        currentFrame' <- currentFrame
        let frameEdges = Map.singleton Lexical (Map.singleton currentScope' currentFrame')
        childFrame <- newFrame classScope frameEdges

        withScopeAndFrame childFrame (void moduleBody)

        moduleSlot <- lookupSlot (Declaration name)
        assign moduleSlot =<< klass (Declaration name) childFrame

        unit

instance Declarations1 Module where
  liftDeclaredName declaredName = declaredName . moduleIdentifier


data LowPrecedenceAnd a = LowPrecedenceAnd { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 LowPrecedenceAnd where liftEq = genericLiftEq
instance Ord1 LowPrecedenceAnd where liftCompare = genericLiftCompare
instance Show1 LowPrecedenceAnd where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LowPrecedenceAnd where
  -- N.B. we have to use Monad rather than Applicative/Traversable on 'And' and 'Or' so that we don't evaluate both operands
  eval eval _ t = go (fmap eval t) where
    go (LowPrecedenceAnd a b) = do
      cond <- a
      ifthenelse cond b (pure cond)


data LowPrecedenceOr a = LowPrecedenceOr { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 LowPrecedenceOr where liftEq = genericLiftEq
instance Ord1 LowPrecedenceOr where liftCompare = genericLiftCompare
instance Show1 LowPrecedenceOr where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LowPrecedenceOr where
  -- N.B. we have to use Monad rather than Applicative/Traversable on 'And' and 'Or' so that we don't evaluate both operands
  eval eval _ t = go (fmap eval t) where
    go (LowPrecedenceOr a b) = do
      cond <- a
      ifthenelse cond (pure cond) b

data Assignment a = Assignment { assignmentContext :: ![a], assignmentTarget :: !a, assignmentValue :: !a }
  deriving (Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Assignment where liftEq = genericLiftEq
instance Ord1 Assignment where liftCompare = genericLiftCompare
instance Show1 Assignment where liftShowsPrec = genericLiftShowsPrec

instance Declarations1 Assignment where
  liftDeclaredName declaredName Assignment{..} = declaredName assignmentTarget

instance Evaluatable Assignment where
  eval eval ref Assignment{..} = do
    (lhsName, relation) <- case declaredName assignmentTarget of
                             Just name -> pure (name, Default)
                             _         -> gensym >>= \name -> pure (name, Gensym)
    maybeSlot <- maybeLookupDeclaration (Declaration lhsName)
    assignmentSpan <- ask @Span
    maybe (declare (Declaration lhsName) relation Public assignmentSpan ScopeGraph.Assignment Nothing) (const (pure ())) maybeSlot

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


-- | A call to @super@ without parentheses in Ruby is known as "zsuper", which has
-- the semantics of invoking @super()@ but implicitly passing the current function's
-- arguments to the @super()@ invocation.
data ZSuper a = ZSuper
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ZSuper where liftEq = genericLiftEq
instance Ord1 ZSuper where liftCompare = genericLiftCompare
instance Show1 ZSuper where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ZSuper
