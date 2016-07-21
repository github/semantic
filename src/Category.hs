module Category where

import Prologue
import Data.Hashable
import Data.String
import Test.QuickCheck

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
  deriving (Eq, Generic, Ord, Show, Typeable)


-- Instances

instance Hashable Category

instance Arbitrary Category where
  arbitrary = oneof
    [ pure Program
    , pure Error
    , pure BinaryOperator
    , pure DictionaryLiteral
    , pure Pair
    , pure FunctionCall
    , pure StringLiteral
    , pure IntegerLiteral
    , pure SymbolLiteral
    , pure ArrayLiteral
    , Other <$> arbitrary
    ]

  shrink (Other s) = Other <$> shrink s
  shrink _ = []
