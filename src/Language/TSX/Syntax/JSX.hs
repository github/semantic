{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveTraversable, DuplicateRecordFields #-}
module Language.TSX.Syntax.JSX (module Language.TSX.Syntax.JSX) where

import Prologue

import           Data.Abstract.Evaluatable
import           Data.JSON.Fields
import qualified Data.Text as T
import           Diffing.Algorithm


data JsxElement a = JsxElement { jsxOpeningElement :: !a,  jsxElements :: ![a], jsxClosingElement :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 JsxElement where liftEq = genericLiftEq
instance Ord1 JsxElement where liftCompare = genericLiftCompare
instance Show1 JsxElement where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable JsxElement

newtype JsxText a = JsxText { contents :: T.Text }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 JsxText where liftEq = genericLiftEq
instance Ord1 JsxText where liftCompare = genericLiftCompare
instance Show1 JsxText where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable JsxText

newtype JsxExpression a = JsxExpression { jsxExpression :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 JsxExpression where liftEq = genericLiftEq
instance Ord1 JsxExpression where liftCompare = genericLiftCompare
instance Show1 JsxExpression where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable JsxExpression

data JsxOpeningElement a = JsxOpeningElement { jsxOpeningElementIdentifier :: !a, jsxOpeningElementTypeArguments :: a, jsxAttributes :: ![a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 JsxOpeningElement where liftEq = genericLiftEq
instance Ord1 JsxOpeningElement where liftCompare = genericLiftCompare
instance Show1 JsxOpeningElement where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable JsxOpeningElement

newtype JsxClosingElement a = JsxClosingElement { jsxClosingElementIdentifier :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 JsxClosingElement where liftEq = genericLiftEq
instance Ord1 JsxClosingElement where liftCompare = genericLiftCompare
instance Show1 JsxClosingElement where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable JsxClosingElement

data JsxSelfClosingElement a = JsxSelfClosingElement { jsxSelfClosingElementIdentifier :: !a, jsxSelfClosingElementAttributes :: ![a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 JsxSelfClosingElement where liftEq = genericLiftEq
instance Ord1 JsxSelfClosingElement where liftCompare = genericLiftCompare
instance Show1 JsxSelfClosingElement where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable JsxSelfClosingElement

data JsxAttribute a = JsxAttribute { jsxAttributeTarget :: !a, jsxAttributeValue :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 JsxAttribute where liftEq = genericLiftEq
instance Ord1 JsxAttribute where liftCompare = genericLiftCompare
instance Show1 JsxAttribute where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable JsxAttribute

newtype JsxFragment a = JsxFragment { terms :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 JsxFragment where liftEq = genericLiftEq
instance Ord1 JsxFragment where liftCompare = genericLiftCompare
instance Show1 JsxFragment where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable JsxFragment

data JsxNamespaceName a = JsxNamespaceName { left :: a, right :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 JsxNamespaceName where liftEq = genericLiftEq
instance Ord1 JsxNamespaceName where liftCompare = genericLiftCompare
instance Show1 JsxNamespaceName where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable JsxNamespaceName
