{-# LANGUAGE DataKinds #-}
module Data.Syntax.Literal where

import Data.Syntax.Comment
import Data.Functor.Union
import Prologue

-- Boolean

newtype Boolean a = Boolean Bool
  deriving (Eq, Show)


-- Numeric

-- | A literal integer of unspecified width. No particular base is implied.
newtype Integer a = Integer { integerContent :: ByteString }

-- TODO: Should IntegerLiteral hold an Integer instead of a ByteString?
-- TODO: Do we care about differentiating between hex/octal/decimal/binary integer literals?
-- TODO: Float/Double literals.


-- Strings, symbols

newtype String a = String { stringElements :: [Union '[InterpolationElement, TextElement] a] }
  deriving (Eq, Show)

-- TODO: Should string literal bodies include escapes too?

-- | An interpolation element within a string literal.
newtype InterpolationElement a = InterpolationElement { interpolationBody :: a }
  deriving (Eq, Show)

-- | A sequence of textual contents within a string literal.
newtype TextElement a = TextElement { textElementContent :: ByteString }
  deriving (Eq, Show)


newtype Symbol a = SymbolLiteral { symbolContent :: ByteString }
  deriving (Eq, Show)

-- TODO: Character literals.


-- Collections

newtype ArrayLiteral a = ArrayLiteral { arrayElements :: [Union '[Identity, Comment] a] }
  deriving (Eq, Show)

newtype HashLiteral a = HashLiteral { hashElements :: [Union '[KeyValue, Comment] a] }
  deriving (Eq, Show)

data KeyValue a = KeyValue { key :: !a, value :: !a }
  deriving (Eq, Show)


-- TODO: Object literals as distinct from hash literals? Or coalesce object/hash literals into “key-value literals”?
-- TODO: Function literals (lambdas, procs, anonymous functions, what have you).
-- TODO: Regexp literals.
