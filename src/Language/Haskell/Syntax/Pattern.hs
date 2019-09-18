{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.Syntax.Pattern where

import Prologue

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm

newtype StrictPattern a = StrictPattern { value :: a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically StrictPattern

instance Evaluatable StrictPattern

newtype ListPattern a = ListPattern { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ListPattern

instance Evaluatable ListPattern

-- e.g. The `n@num1` in `f n@num1 x@num2 = x`
data AsPattern a = AsPattern { asPatternLeft :: a, asPatternRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically AsPattern

instance Evaluatable AsPattern

newtype TypePattern a = TypePattern { typePatternContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TypePattern

instance Evaluatable TypePattern

-- e.g. The `a = 1` in `foo Bar{ a = 1 } = baz`.
data FieldPattern a = FieldPattern { fieldPatternLeft :: a, fieldPatternRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically FieldPattern

instance Evaluatable FieldPattern

-- e.g. The `~a` in `f ~a = 1`
newtype IrrefutablePattern a = IrrefutablePattern { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically IrrefutablePattern

instance Evaluatable IrrefutablePattern

-- For handling guards in case alternative expressions.
newtype CaseGuardPattern a = CaseGuardPattern { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically CaseGuardPattern

instance Evaluatable CaseGuardPattern

-- For handling guards in function declarations.
newtype FunctionGuardPattern a = FunctionGuardPattern { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically FunctionGuardPattern

instance Evaluatable FunctionGuardPattern

data ViewPattern a = ViewPattern { viewPatternLeft :: a, viewPatternRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ViewPattern

instance Evaluatable ViewPattern

-- e.g. The `Bar{..}` in `foo Bar{..} = baz`.
newtype LabeledPattern a = LabeledPattern { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically LabeledPattern

instance Evaluatable LabeledPattern

-- The `a <- b` in `f a | a <- b = c` of a function declaration.
data PatternGuard a = PatternGuard { patternGuardPattern :: a, patternGuardExpression :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically PatternGuard

instance Evaluatable PatternGuard

newtype Guard a = Guard { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Guard

instance Evaluatable Guard
