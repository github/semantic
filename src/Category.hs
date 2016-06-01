{-# LANGUAGE FlexibleInstances #-}
module Category where

import Prologue
import Data.String
import Term

-- | A standardized category of AST node. Used to determine the semantics for
-- | semantic diffing and define comparability of nodes.
data Category =
  -- | An operator with 2 operands.
  BinaryOperator
  -- | A literal key-value data structure.
  | DictionaryLiteral
  -- | A pair, e.g. of a key & value
  | Pair
  -- | A call to a function.
  | FunctionCall
  -- | A string literal.
  | StringLiteral
  -- | An integer literal.
  | IntegerLiteral
  -- | A symbol literal.
  | SymbolLiteral
  -- | An array literal.
  | ArrayLiteral
  -- | A non-standard category, which can be used for comparability.
  | Other String
  deriving (Eq, Show, Ord)

-- | The class of types that have categories.
class Categorizable a where
  category :: a -> Category

instance Categorizable annotation => Categorizable (Term a annotation) where
  category term | (annotation :< _) <- runCofree term = category annotation
