{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.TypeScript.Syntax.JSX where

import Prologue

import           Control.Abstract as Abstract
import           Data.Abstract.Evaluatable
import           Data.Abstract.ScopeGraph (AccessControl(..))
import           Data.JSON.Fields
import qualified Data.Text as T
import           Diffing.Algorithm
import qualified Data.Map.Strict as Map
import Control.Abstract as Abstract
import qualified Data.Abstract.ScopeGraph as ScopeGraph

data JsxElement a = JsxElement { jsxOpeningElement :: !a,  jsxElements :: ![a], jsxClosingElement :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxElement

instance Evaluatable JsxElement

newtype JsxText a = JsxText { contents :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxText

instance Evaluatable JsxText

newtype JsxExpression a = JsxExpression { jsxExpression :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxExpression

instance Evaluatable JsxExpression

data JsxOpeningElement a = JsxOpeningElement { jsxOpeningElementIdentifier :: !a,  jsxAttributes :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxOpeningElement

instance Evaluatable JsxOpeningElement

newtype JsxClosingElement a = JsxClosingElement { jsxClosingElementIdentifier :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxClosingElement

instance Evaluatable JsxClosingElement

data JsxSelfClosingElement a = JsxSelfClosingElement { jsxSelfClosingElementIdentifier :: !a, jsxSelfClosingElementAttributes :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxSelfClosingElement

instance Evaluatable JsxSelfClosingElement

data JsxAttribute a = JsxAttribute { jsxAttributeTarget :: !a, jsxAttributeValue :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxAttribute

instance Evaluatable JsxAttribute

newtype ImplementsClause a = ImplementsClause { implementsClauseTypes :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ImplementsClause

instance Evaluatable ImplementsClause

data OptionalParameter a = OptionalParameter { optionalParameterContext :: ![a], optionalParameterSubject :: !a, optionalParameterAccessControl :: AccessControl }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically OptionalParameter

instance Evaluatable OptionalParameter

data RequiredParameter a = RequiredParameter { requiredParameterContext :: [a], requiredParameterSubject :: a, requiredParameterValue :: a, requiredParameterAccessControl :: AccessControl }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically RequiredParameter

instance Declarations1 RequiredParameter where
  liftDeclaredName declaredName RequiredParameter{..} = declaredName requiredParameterSubject

instance Evaluatable RequiredParameter where
  eval eval ref RequiredParameter{..} = do
    name <- maybeM (throwNoNameError requiredParameterSubject) (declaredName requiredParameterSubject)
    span <- ask @Span
    declare (Declaration name) Default Public span Nothing

    lhs <- ref requiredParameterSubject
    rhs <- eval requiredParameterValue

    case declaredName requiredParameterValue of
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

data RestParameter a = RestParameter { restParameterContext :: ![a], restParameterSubject :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically RestParameter

instance Evaluatable RestParameter

newtype JsxFragment a = JsxFragment { terms :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxFragment

instance Evaluatable JsxFragment

data JsxNamespaceName a = JsxNamespaceName { left :: a, right :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxNamespaceName

instance Evaluatable JsxNamespaceName
