{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.Syntax.QuasiQuote where

import Prologue

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm

data QuasiQuotation a = QuasiQuotation { quasiQuotationHead :: a, quasiQuotationBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QuasiQuotation

instance Evaluatable QuasiQuotation

newtype QuasiQuotationExpressionBody a = QuasiQuotationExpressionBody { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QuasiQuotationExpressionBody

instance Evaluatable QuasiQuotationExpressionBody

data QuasiQuotationPattern a = QuasiQuotationPattern
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QuasiQuotationPattern

instance Evaluatable QuasiQuotationPattern

data QuasiQuotationType a = QuasiQuotationType
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QuasiQuotationType

instance Evaluatable QuasiQuotationType

data QuasiQuotationDeclaration a = QuasiQuotationDeclaration
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QuasiQuotationDeclaration

instance Evaluatable QuasiQuotationDeclaration

newtype QuasiQuotationQuoter a = QuasiQuotationQuoter { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QuasiQuotationQuoter

instance Evaluatable QuasiQuotationQuoter

data QuasiQuotationExpression a = QuasiQuotationExpression
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QuasiQuotationExpression

instance Evaluatable QuasiQuotationExpression

newtype Splice a = Splice { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Splice

instance Evaluatable Splice
