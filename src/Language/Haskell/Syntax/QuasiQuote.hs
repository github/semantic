{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.Syntax.QuasiQuote where

import Prologue

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm
import Proto3.Suite.Class

data QuasiQuotation a = QuasiQuotation { quasiQuotationHead :: a, quasiQuotationBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 QuasiQuotation where liftEq = genericLiftEq
instance Ord1 QuasiQuotation where liftCompare = genericLiftCompare
instance Show1 QuasiQuotation where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuasiQuotation

newtype QuasiQuotationExpressionBody a = QuasiQuotationExpressionBody { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 QuasiQuotationExpressionBody where liftEq = genericLiftEq
instance Ord1 QuasiQuotationExpressionBody where liftCompare = genericLiftCompare
instance Show1 QuasiQuotationExpressionBody where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuasiQuotationExpressionBody

data QuasiQuotationPattern a = QuasiQuotationPattern
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 QuasiQuotationPattern where liftEq = genericLiftEq
instance Ord1 QuasiQuotationPattern where liftCompare = genericLiftCompare
instance Show1 QuasiQuotationPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuasiQuotationPattern

data QuasiQuotationType a = QuasiQuotationType
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 QuasiQuotationType where liftEq = genericLiftEq
instance Ord1 QuasiQuotationType where liftCompare = genericLiftCompare
instance Show1 QuasiQuotationType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuasiQuotationType

data QuasiQuotationDeclaration a = QuasiQuotationDeclaration
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 QuasiQuotationDeclaration where liftEq = genericLiftEq
instance Ord1 QuasiQuotationDeclaration where liftCompare = genericLiftCompare
instance Show1 QuasiQuotationDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuasiQuotationDeclaration

newtype QuasiQuotationQuoter a = QuasiQuotationQuoter { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 QuasiQuotationQuoter where liftEq = genericLiftEq
instance Ord1 QuasiQuotationQuoter where liftCompare = genericLiftCompare
instance Show1 QuasiQuotationQuoter where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuasiQuotationQuoter

data QuasiQuotationExpression a = QuasiQuotationExpression
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 QuasiQuotationExpression where liftEq = genericLiftEq
instance Ord1 QuasiQuotationExpression where liftCompare = genericLiftCompare
instance Show1 QuasiQuotationExpression where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuasiQuotationExpression

newtype Splice a = Splice { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Splice where liftEq = genericLiftEq
instance Ord1 Splice where liftCompare = genericLiftCompare
instance Show1 Splice where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Splice
