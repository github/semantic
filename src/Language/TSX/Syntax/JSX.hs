{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.TSX.Syntax.JSX where

import Prologue

import           Data.Abstract.Evaluatable
import           Data.JSON.Fields
import qualified Data.Text as T
import           Diffing.Algorithm
import           Tags.Taggable (Taggable)


data JsxElement a = JsxElement { jsxOpeningElement :: !a,  jsxElements :: ![a], jsxClosingElement :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, Taggable, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxElement

instance Evaluatable JsxElement

newtype JsxText a = JsxText { contents :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, Taggable, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxText

instance Evaluatable JsxText

newtype JsxExpression a = JsxExpression { jsxExpression :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, Taggable, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxExpression

instance Evaluatable JsxExpression

data JsxOpeningElement a = JsxOpeningElement { jsxOpeningElementIdentifier :: !a, jsxOpeningElementTypeArguments :: a, jsxAttributes :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, Taggable, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxOpeningElement

instance Evaluatable JsxOpeningElement

newtype JsxClosingElement a = JsxClosingElement { jsxClosingElementIdentifier :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, Taggable, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxClosingElement

instance Evaluatable JsxClosingElement

data JsxSelfClosingElement a = JsxSelfClosingElement { jsxSelfClosingElementIdentifier :: !a, jsxSelfClosingElementAttributes :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, Taggable, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxSelfClosingElement

instance Evaluatable JsxSelfClosingElement

data JsxAttribute a = JsxAttribute { jsxAttributeTarget :: !a, jsxAttributeValue :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, Taggable, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxAttribute

instance Evaluatable JsxAttribute

newtype JsxFragment a = JsxFragment { terms :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, Taggable, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxFragment

instance Evaluatable JsxFragment

data JsxNamespaceName a = JsxNamespaceName { left :: a, right :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, Taggable, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JsxNamespaceName

instance Evaluatable JsxNamespaceName
