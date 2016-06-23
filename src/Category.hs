module Category where

import Prologue

-- | A standardized category of AST node. Used to determine the semantics for
-- | semantic diffing and define comparability of nodes.
data Category
  -- | The top-level branch node.
  = Program
  -- | A node indicating syntax errors.
  | Error
  -- | A boolean expression.
  | Boolean
  -- | An operator with 2 operands.
  | BinaryOperator
  -- | A literal key-value data structure.
  | DictionaryLiteral
  -- | A pair, e.g. of a key & value
  | Pair
  -- | A call to a function.
  | FunctionCall
  -- | A function declaration.
  | Function
  -- | An identifier.
  | Identifier
  -- | A function's parameters.
  | Params
  -- | A function's expression statements.
  | ExpressionStatements
  -- | A method call on an object.
  | MethodCall
  -- | A method's arguments.
  | Args
  -- | A string literal.
  | StringLiteral
  -- | An integer literal.
  | IntegerLiteral
  -- | A regex literal.
  | Regex
  -- | A symbol literal.
  | SymbolLiteral
  -- | A template string literal.
  | TemplateString
  -- | An array literal.
  | ArrayLiteral
  -- | An assignment expression.
  | Assignment
  -- | A math assignment expression.
  | MathAssignment
  -- | A member access expression.
  | MemberAccess
  -- | A subscript access expression.
  | SubscriptAccess
  -- | A variable assignment within a variable declaration.
  | VarAssignment
  -- | A variable declaration.
  | VarDecl
  -- | A switch expression.
  | Switch
  -- | A ternary expression.
  | Ternary
  -- | A case expression.
  | Case
  -- | An expression with an operator.
  | Operator
  -- | An object/dictionary/hash literal.
  | Object
  -- | A non-standard category, which can be used for comparability.
  | Other Text
  deriving (Eq, Show, Ord)
