{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, MultiParamTypeClasses #-}
module Data.Syntax.Literal where

import Data.Abstract.Evaluatable
import Data.ByteString.Char8 (readInteger)
import Diffing.Algorithm
import Prologue hiding (Set)

-- Boolean

newtype Boolean a = Boolean Bool
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

true :: Boolean a
true = Boolean True

false :: Boolean a
false = Boolean False

instance Eq1 Boolean where liftEq = genericLiftEq
instance Ord1 Boolean where liftCompare = genericLiftCompare
instance Show1 Boolean where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Boolean where
  eval (Boolean x) = pure (boolean x)


-- Numeric

-- | A literal integer of unspecified width. No particular base is implied.
newtype Integer a = Integer { integerContent :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Data.Syntax.Literal.Integer where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Literal.Integer where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Literal.Integer where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Data.Syntax.Literal.Integer where
  -- TODO: This instance probably shouldn't have readInteger?
  eval (Data.Syntax.Literal.Integer x) = pure (integer (maybe 0 fst (readInteger x)))


-- TODO: Should IntegerLiteral hold an Integer instead of a ByteString?
-- TODO: Do we care about differentiating between hex/octal/decimal/binary integer literals?
-- TODO: Consider a Numeric datatype with FloatingPoint/Integral/etc constructors.

-- | A literal float of unspecified width.
newtype Float a = Float { floatContent :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Data.Syntax.Literal.Float where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Literal.Float where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Literal.Float where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Float
instance Evaluatable Data.Syntax.Literal.Float


-- Rational literals e.g. `2/3r`
newtype Rational a = Rational ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Data.Syntax.Literal.Rational where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Literal.Rational where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Literal.Rational where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Rational
instance Evaluatable Data.Syntax.Literal.Rational


-- Complex literals e.g. `3 + 2i`
newtype Complex a = Complex ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Data.Syntax.Literal.Complex where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Literal.Complex where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Literal.Complex where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Complex
instance Evaluatable Complex

-- Strings, symbols

newtype String a = String { stringElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Data.Syntax.Literal.String where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Literal.String where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Literal.String where liftShowsPrec = genericLiftShowsPrec

-- TODO: Should string literal bodies include escapes too?

-- TODO: Implement Eval instance for String
instance Evaluatable Data.Syntax.Literal.String


-- | An interpolation element within a string literal.
newtype InterpolationElement a = InterpolationElement { interpolationBody :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 InterpolationElement where liftEq = genericLiftEq
instance Ord1 InterpolationElement where liftCompare = genericLiftCompare
instance Show1 InterpolationElement where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for InterpolationElement
instance Evaluatable InterpolationElement


-- | A sequence of textual contents within a string literal.
newtype TextElement a = TextElement { textElementContent :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 TextElement where liftEq = genericLiftEq
instance Ord1 TextElement where liftCompare = genericLiftCompare
instance Show1 TextElement where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TextElement where
  eval (TextElement x) = pure (string x)

data Null a = Null
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Null where liftEq = genericLiftEq
instance Ord1 Null where liftCompare = genericLiftCompare
instance Show1 Null where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Null
instance Evaluatable Null


newtype Symbol a = Symbol { symbolContent :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Symbol where liftEq = genericLiftEq
instance Ord1 Symbol where liftCompare = genericLiftCompare
instance Show1 Symbol where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Symbol
instance Evaluatable Symbol


newtype Regex a = Regex { regexContent :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Regex where liftEq = genericLiftEq
instance Ord1 Regex where liftCompare = genericLiftCompare
instance Show1 Regex where liftShowsPrec = genericLiftShowsPrec

-- TODO: Heredoc-style string literals?
-- TODO: Character literals.

-- TODO: Implement Eval instance for Regex
instance Evaluatable Regex


-- Collections

newtype Array a = Array { arrayElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Array where liftEq = genericLiftEq
instance Ord1 Array where liftCompare = genericLiftCompare
instance Show1 Array where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Array
instance Evaluatable Array


newtype Hash a = Hash { hashElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Hash where liftEq = genericLiftEq
instance Ord1 Hash where liftCompare = genericLiftCompare
instance Show1 Hash where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Hash
instance Evaluatable Hash


data KeyValue a = KeyValue { key :: !a, value :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 KeyValue where liftEq = genericLiftEq
instance Ord1 KeyValue where liftCompare = genericLiftCompare
instance Show1 KeyValue where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for KeyValue
instance Evaluatable KeyValue


newtype Tuple a = Tuple { tupleContents :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Tuple where liftEq = genericLiftEq
instance Ord1 Tuple where liftCompare = genericLiftCompare
instance Show1 Tuple where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Tuple
instance Evaluatable Tuple


newtype Set a = Set { setElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Set where liftEq = genericLiftEq
instance Ord1 Set where liftCompare = genericLiftCompare
instance Show1 Set where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Set
instance Evaluatable Set


-- Pointers

-- | A declared pointer (e.g. var pointer *int in Go)
newtype Pointer a = Pointer a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Pointer where liftEq = genericLiftEq
instance Ord1 Pointer where liftCompare = genericLiftCompare
instance Show1 Pointer where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Pointer
instance Evaluatable Pointer


-- | A reference to a pointer's address (e.g. &pointer in Go)
newtype Reference a = Reference a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Reference where liftEq = genericLiftEq
instance Ord1 Reference where liftCompare = genericLiftCompare
instance Show1 Reference where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Reference
instance Evaluatable Reference

-- TODO: Object literals as distinct from hash literals? Or coalesce object/hash literals into “key-value literals”?
-- TODO: Function literals (lambdas, procs, anonymous functions, what have you).
