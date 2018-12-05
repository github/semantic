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
import           Data.Abstract.Path
import           Data.JSON.Fields
import           Diffing.Algorithm
import           Proto3.Suite.Class
import           Reprinting.Tokenize
import Control.Abstract as Abstract hiding (Load)
import Control.Abstract.Heap (insertFrameLink, HeapError, Heap)
import Control.Abstract.ScopeGraph (insertImportEdge)
import Data.Abstract.Name as Name
import qualified Data.Abstract.Module as M
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import qualified Data.Language as Language
import qualified Data.Map.Strict as Map
import qualified Data.Reprinting.Scope as Scope
import Data.Semigroup.App
import Data.Semigroup.Foldable

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
  eval eval Send{..} = do
    sel <- case sendSelector of
             Just sel -> maybeM (throwEvalError NoNameError) (declaredName sel)
             Nothing  ->
               -- TODO: if there is no selector then it's a call on the receiver
               -- Previously we returned a variable called `call`.
               throwEvalError NoNameError

    let self = LvalMember <$> lookupDeclaration (Declaration $ Name.name "__self")
    recv <- maybe self eval sendReceiver
    lhsValue <- Abstract.value recv
    lhsFrame <- Abstract.scopedEnvironment lhsValue

    let callFunction = do
          reference (Reference sel) (Declaration sel)
          func <- deref =<< lookupDeclaration (Declaration sel)
          args <- traverse (eval >=> Abstract.value) sendArgs
          call func (lhsValue : args) -- TODO pass through sendBlock
    maybe callFunction (flip withScopeAndFrame callFunction) lhsFrame

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
    ((moduleScope, moduleFrame), v) <- doRequire path
    insertImportEdge moduleScope
    insertFrameLink ScopeGraph.Import (Map.singleton moduleScope moduleFrame)
    rvalBox v -- Returns True if the file was loaded, False if it was already loaded. http://ruby-doc.org/core-2.5.0/Kernel.html#method-i-require

doRequire :: ( Member (Boolean value) sig
             , Member (Modules address value) sig
             , Carrier sig m
             )
          => M.ModulePath
          -> Evaluator term address value m ((address, address), value)
doRequire path = do
  result <- lookupModule path
  case result of
    Nothing       -> (,) . fst <$> load path <*> boolean True
    Just (scopeAndFrame, _) -> (scopeAndFrame, ) <$> boolean False


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
          , Member (State (ScopeGraph.ScopeGraph address)) sig
          , Member (State (Heap address address value)) sig
          , Member (Resumable (BaseError (HeapError address))) sig
          , Member (Reader (address, address)) sig
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

instance Diffable Class where
  equivalentBySubterm = Just . classIdentifier

instance Eq1 Class where liftEq = genericLiftEq
instance Ord1 Class where liftCompare = genericLiftCompare
instance Show1 Class where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Class where
  eval eval Class{..} = do
    name <- maybeM (throwEvalError NoNameError) (declaredName classIdentifier)
    span <- ask @Span
    currentScope' <- currentScope

    let declaration = (Declaration name)
    maybeSlot <- maybeLookupDeclaration declaration

    case maybeSlot of
      Just slot -> do
        classVal <- deref slot
        maybeFrame <- scopedEnvironment classVal
        case maybeFrame of
          Just classFrame -> withScopeAndFrame classFrame (eval classBody)
          Nothing -> throwEvalError (DerefError classVal)
      Nothing -> do
        let classSuperclasses = maybeToList classSuperClass
        superScopes <- for classSuperclasses $ \superclass -> do
          name <- maybeM (throwEvalError NoNameError) (declaredName superclass)
          scope <- associatedScope (Declaration name)
          slot <- lookupDeclaration (Declaration name)
          superclassFrame <- scopedEnvironment =<< deref slot
          pure $ case (scope, superclassFrame) of
            (Just scope, Just frame) -> Just (scope, frame)
            _ -> Nothing

        let superclassEdges = fmap (Superclass, ) . fmap (pure . fst) . catMaybes $ superScopes
            current = fmap (Lexical, ) . pure . pure $ currentScope'
            edges = Map.fromList (superclassEdges <> current)
        childScope <- newScope edges
        declare (Declaration name) span (Just childScope)

        let frameEdges = Map.singleton Superclass (Map.fromList (catMaybes superScopes))
        childFrame <- newFrame childScope frameEdges

        withScopeAndFrame childFrame $ do
          void $ eval classBody

        classSlot <- lookupDeclaration (Declaration name)
        assign classSlot =<< klass (Declaration name) childFrame

        rvalBox unit

instance Declarations1 Class where
  liftDeclaredName declaredName = declaredName . classIdentifier

data Module a = Module { moduleIdentifier :: !a, moduleStatements :: ![a] }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Module where
  eval eval Module{..} =  do
    name <- maybeM (throwEvalError NoNameError) (declaredName moduleIdentifier)
    span <- ask @Span
    currentScope' <- currentScope

    let declaration = (Declaration name)
        moduleBody = maybe (rvalBox unit) (runApp . foldMap1 (App . eval)) (nonEmpty moduleStatements)
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
        childScope <- newScope edges
        declare (Declaration name) span (Just childScope)

        currentFrame' <- currentFrame
        let frameEdges = Map.singleton Lexical (Map.singleton currentScope' currentFrame')
        childFrame <- newFrame childScope frameEdges

        withScopeAndFrame childFrame (void moduleBody)

        moduleSlot <- lookupDeclaration (Declaration name)
        assign moduleSlot =<< klass (Declaration name) childFrame

        rvalBox unit

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

data Assignment a = Assignment { assignmentContext :: ![a], assignmentTarget :: !a, assignmentValue :: !a }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Declarations1 Assignment where
  liftDeclaredName declaredName Assignment{..} = declaredName assignmentTarget

instance Eq1 Assignment where liftEq = genericLiftEq
instance Ord1 Assignment where liftCompare = genericLiftCompare
instance Show1 Assignment where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Assignment where
  eval eval Assignment{..} = do
    lhsName <- maybeM (throwEvalError NoNameError) (declaredName assignmentTarget)
    maybeSlot <- maybeLookupDeclaration (Declaration lhsName)
    assignmentSpan <- ask @Span
    maybe (declare (Declaration lhsName) assignmentSpan Nothing) (const (pure ())) maybeSlot

    lhs <- eval assignmentTarget
    rhs <- eval assignmentValue

    case lhs of
      Rval val -> throwEvalError (DerefError val)
      LvalMember lhsSlot -> do
        case declaredName assignmentValue of
          Just rhsName -> do
            assocScope <- associatedScope (Declaration rhsName)
            case assocScope of
              Just assocScope' -> do
                objectScope <- newScope (Map.singleton Import [ assocScope' ])
                putSlotDeclarationScope lhsSlot (Just objectScope) -- TODO: not sure if this is right
              Nothing ->
                pure ()
          Nothing ->
            pure ()
        assign lhsSlot =<< Abstract.value rhs
        pure (LvalMember lhsSlot)
