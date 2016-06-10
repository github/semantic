module Category where

import Prologue

-- | A standardized category of AST node. Used to determine the semantics for
-- | semantic diffing and define comparability of nodes.
data Category
  -- | The top-level branch node.
  = Program
  -- | A node indicating syntax errors.
  | Error
  -- | An operator with 2 operands.
  | BinaryOperator
  -- | A literal key-value data structure.
  | DictionaryLiteral
  -- | A pair, e.g. of a key & value
  | Pair
  -- | A call to a function.
  | FunctionCall
  -- | A function declaration
  | Function
  | Params
  | ExpressionStatements
  -- | A string literal.
  | StringLiteral
  -- | An integer literal.
  | IntegerLiteral
  -- | A symbol literal.
  | SymbolLiteral
  -- | An array literal.
  | ArrayLiteral
  -- | A non-standard category, which can be used for comparability.
  | Other Text
  deriving (Eq, Show, Ord)
