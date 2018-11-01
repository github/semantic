{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.Syntax.Pattern where

import Prologue

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm
import Proto3.Suite.Class

newtype StrictPattern a = StrictPattern { value :: a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 StrictPattern where liftEq = genericLiftEq
instance Ord1 StrictPattern where liftCompare = genericLiftCompare
instance Show1 StrictPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable StrictPattern

newtype ListPattern a = ListPattern { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ListPattern where liftEq = genericLiftEq
instance Ord1 ListPattern where liftCompare = genericLiftCompare
instance Show1 ListPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ListPattern

-- e.g. The `n@num1` in `f n@num1 x@num2 = x`
data AsPattern a = AsPattern { asPatternLeft :: a, asPatternRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 AsPattern where liftEq = genericLiftEq
instance Ord1 AsPattern where liftCompare = genericLiftCompare
instance Show1 AsPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable AsPattern

newtype TypePattern a = TypePattern { typePatternContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TypePattern where liftEq = genericLiftEq
instance Ord1 TypePattern where liftCompare = genericLiftCompare
instance Show1 TypePattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypePattern

-- e.g. The `a = 1` in `foo Bar{ a = 1 } = baz`.
data FieldPattern a = FieldPattern { fieldPatternLeft :: a, fieldPatternRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 FieldPattern where liftEq = genericLiftEq
instance Ord1 FieldPattern where liftCompare = genericLiftCompare
instance Show1 FieldPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FieldPattern

-- e.g. The `~a` in `f ~a = 1`
newtype IrrefutablePattern a = IrrefutablePattern { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 IrrefutablePattern where liftEq = genericLiftEq
instance Ord1 IrrefutablePattern where liftCompare = genericLiftCompare
instance Show1 IrrefutablePattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable IrrefutablePattern

-- For handling guards in case alternative expressions.
newtype CaseGuardPattern a = CaseGuardPattern { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 CaseGuardPattern where liftEq = genericLiftEq
instance Ord1 CaseGuardPattern where liftCompare = genericLiftCompare
instance Show1 CaseGuardPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable CaseGuardPattern

-- For handling guards in function declarations.
newtype FunctionGuardPattern a = FunctionGuardPattern { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 FunctionGuardPattern where liftEq = genericLiftEq
instance Ord1 FunctionGuardPattern where liftCompare = genericLiftCompare
instance Show1 FunctionGuardPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FunctionGuardPattern

data ViewPattern a = ViewPattern { viewPatternLeft :: a, viewPatternRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ViewPattern where liftEq = genericLiftEq
instance Ord1 ViewPattern where liftCompare = genericLiftCompare
instance Show1 ViewPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ViewPattern

-- e.g. The `Bar{..}` in `foo Bar{..} = baz`.
newtype LabeledPattern a = LabeledPattern { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 LabeledPattern where liftEq = genericLiftEq
instance Ord1 LabeledPattern where liftCompare = genericLiftCompare
instance Show1 LabeledPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LabeledPattern

-- The `a <- b` in `f a | a <- b = c` of a function declaration.
data PatternGuard a = PatternGuard { patternGuardPattern :: a, patternGuardExpression :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 PatternGuard where liftEq = genericLiftEq
instance Ord1 PatternGuard where liftCompare = genericLiftCompare
instance Show1 PatternGuard where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable PatternGuard

newtype Guard a = Guard { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Guard where liftEq = genericLiftEq
instance Ord1 Guard where liftCompare = genericLiftCompare
instance Show1 Guard where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Guard
