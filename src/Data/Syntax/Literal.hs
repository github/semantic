{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric #-}
module Data.Syntax.Literal where

import Data.Functor.Classes.Eq.Generic
import Data.Syntax.Comment
import Data.Functor.Union
import GHC.Generics
import Prologue

-- Boolean

newtype Boolean a = Boolean Bool
  deriving (Eq, Generic1, Show)

instance Eq1 Boolean where liftEq = genericLiftEq


-- Numeric

-- | A literal integer of unspecified width. No particular base is implied.
newtype Integer a = Integer { integerContent :: ByteString }
  deriving (Eq, Generic1, Show)

instance Eq1 Data.Syntax.Literal.Integer where liftEq = genericLiftEq

-- TODO: Should IntegerLiteral hold an Integer instead of a ByteString?
-- TODO: Do we care about differentiating between hex/octal/decimal/binary integer literals?
-- TODO: Float/Double literals.


-- Strings, symbols

newtype String a = String { stringElements :: [Union '[InterpolationElement, TextElement] a] }
  deriving (Eq, Generic1, Show)

instance Eq1 String where liftEq = genericLiftEq

-- TODO: Should string literal bodies include escapes too?

-- | An interpolation element within a string literal.
newtype InterpolationElement a = InterpolationElement { interpolationBody :: a }
  deriving (Eq, Generic1, Show)

instance Eq1 InterpolationElement where liftEq = genericLiftEq


-- | A sequence of textual contents within a string literal.
newtype TextElement a = TextElement { textElementContent :: ByteString }
  deriving (Eq, Generic1, Show)

instance Eq1 TextElement where liftEq = genericLiftEq


newtype Symbol a = SymbolLiteral { symbolContent :: ByteString }
  deriving (Eq, Generic1, Show)

instance Eq1 Symbol where liftEq = genericLiftEq

-- TODO: Character literals.


-- Collections

newtype ArrayLiteral a = ArrayLiteral { arrayElements :: [Union '[Identity, Comment] a] }
  deriving (Eq, Generic1, Show)

instance Eq1 ArrayLiteral where liftEq = genericLiftEq


newtype HashLiteral a = HashLiteral { hashElements :: [Union '[KeyValue, Comment] a] }
  deriving (Eq, Generic1, Show)

instance Eq1 HashLiteral where liftEq = genericLiftEq


data KeyValue a = KeyValue { key :: !a, value :: !a }
  deriving (Eq, Generic1, Show)

instance Eq1 KeyValue where liftEq = genericLiftEq

-- TODO: Object literals as distinct from hash literals? Or coalesce object/hash literals into “key-value literals”?
-- TODO: Function literals (lambdas, procs, anonymous functions, what have you).
-- TODO: Regexp literals.
