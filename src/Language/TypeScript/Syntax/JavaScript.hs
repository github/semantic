{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, RecordWildCards, TypeApplications #-}
module Language.TypeScript.Syntax.JavaScript (module Language.TypeScript.Syntax.JavaScript) where

import Prologue

import           Control.Abstract.Heap
import           Control.Abstract.ScopeGraph hiding (Import)
import           Data.Abstract.Evaluatable
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.JSON.Fields
import qualified Data.Map.Strict as Map
import           Diffing.Algorithm
import           Language.TypeScript.Resolution

newtype ImplementsClause a = ImplementsClause { implementsClauseTypes :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ImplementsClause where liftEq = genericLiftEq
instance Ord1 ImplementsClause where liftCompare = genericLiftCompare
instance Show1 ImplementsClause where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ImplementsClause

data OptionalParameter a = OptionalParameter { optionalParameterContext :: ![a], optionalParameterSubject :: !a, optionalParameterAccessControl :: AccessControl }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 OptionalParameter where liftEq = genericLiftEq
instance Ord1 OptionalParameter where liftCompare = genericLiftCompare
instance Show1 OptionalParameter where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable OptionalParameter

data RequiredParameter a = RequiredParameter { requiredParameterContext :: [a], requiredParameterSubject :: a, requiredParameterValue :: a, requiredParameterAccessControl :: AccessControl }
  deriving (Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 RequiredParameter where liftEq = genericLiftEq
instance Ord1 RequiredParameter where liftCompare = genericLiftCompare
instance Show1 RequiredParameter where liftShowsPrec = genericLiftShowsPrec

instance Declarations1 RequiredParameter where
  liftDeclaredName declaredName RequiredParameter{..} = declaredName requiredParameterSubject

instance Evaluatable RequiredParameter where
  eval eval ref RequiredParameter{..} = do
    span <- ask @Span
    _ <- declareMaybeName (declaredName requiredParameterSubject) Default Public span ScopeGraph.RequiredParameter Nothing

    lhs <- ref requiredParameterSubject
    rhs <- eval requiredParameterValue

    case declaredName requiredParameterValue of
      Just rhsName -> do
        assocScope <- associatedScope (Declaration rhsName)
        case assocScope of
          Just assocScope' -> do
            objectScope <- newScope (Map.singleton ScopeGraph.Import [ assocScope' ])
            putSlotDeclarationScope lhs (Just objectScope) -- TODO: not sure if this is right
          Nothing ->
            pure ()
      Nothing ->
        pure ()
    assign lhs rhs
    pure rhs

data RestParameter a = RestParameter { restParameterContext :: ![a], restParameterSubject :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 RestParameter where liftEq = genericLiftEq
instance Ord1 RestParameter where liftCompare = genericLiftCompare
instance Show1 RestParameter where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable RestParameter


data JavaScriptRequire a = JavaScriptRequire { javascriptRequireIden :: !a, javascriptRequireFrom :: ImportPath }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 JavaScriptRequire where liftEq = genericLiftEq
instance Ord1 JavaScriptRequire where liftCompare = genericLiftCompare
instance Show1 JavaScriptRequire where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable JavaScriptRequire where
  eval _ _ (JavaScriptRequire aliasTerm importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath javascriptExtensions
    ((moduleScope, moduleFrame), _) <- require modulePath

    case declaredName aliasTerm of
      Just alias -> do
        span <- ask @Span
        importScope <- newScope (Map.singleton ScopeGraph.Import [ moduleScope ])
        declare (Declaration alias) Default Public span ScopeGraph.UnqualifiedImport (Just importScope)
        let scopeMap = Map.singleton moduleScope moduleFrame
        aliasFrame <- newFrame importScope (Map.singleton ScopeGraph.Import scopeMap)
        aliasSlot <- lookupSlot (Declaration alias)
        assign aliasSlot =<< object aliasFrame
      Nothing -> do
        insertImportEdge moduleScope
        insertFrameLink ScopeGraph.Import (Map.singleton moduleScope moduleFrame)
    unit

data Debugger a = Debugger
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Debugger where liftEq = genericLiftEq
instance Ord1 Debugger where liftCompare = genericLiftCompare
instance Show1 Debugger where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Debugger

data Super a = Super
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Super where liftEq = genericLiftEq
instance Ord1 Super where liftCompare = genericLiftCompare
instance Show1 Super where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Super

data Undefined a = Undefined
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Undefined where liftEq = genericLiftEq
instance Ord1 Undefined where liftCompare = genericLiftCompare
instance Show1 Undefined where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Undefined

data With a = With { withExpression :: !a, withBody :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 With where liftEq = genericLiftEq
instance Ord1 With where liftCompare = genericLiftCompare
instance Show1 With where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable With

-- | A sequence expression such as Javascript or C's comma operator.
data AnnotatedExpression a = AnnotatedExpression { expression :: !a, typeAnnotation :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 AnnotatedExpression where liftEq = genericLiftEq
instance Ord1 AnnotatedExpression where liftCompare = genericLiftCompare
instance Show1 AnnotatedExpression where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable AnnotatedExpression where
  eval eval _ (AnnotatedExpression a b) = eval b >> eval a
