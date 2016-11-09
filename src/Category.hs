{-# OPTIONS_GHC -funbox-strict-fields #-}
module Category where

import Prologue
import Test.QuickCheck hiding (Args)
import Data.Text.Arbitrary()

-- | A standardized category of AST node. Used to determine the semantics for
-- | semantic diffing and define comparability of nodes.
data Category
  -- | The top-level branch node.
  = Program
  -- | A node indicating syntax errors.
  | Error
  -- | A boolean expression.
  | Boolean
  -- | A bitwise operator.
  | BitwiseOperator
  -- | A boolean operator (e.g. ||, &&).
  | BooleanOperator
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
  -- | A return statement.
  | Return
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
  -- | A if/else expression.
  | If
  -- | A for expression.
  | For
  -- | A while expression.
  | While
  -- | A do/while expression.
  | DoWhile
  -- | A ternary expression.
  | Ternary
  -- | A case expression.
  | Case
  -- | An expression with an operator.
  | Operator
  -- | An comma operator expression
  | CommaOperator
  -- | An object/dictionary/hash literal.
  | Object
  -- | A throw statement.
  | Throw
  -- | A constructor statement, e.g. new Foo;
  | Constructor
  -- | A try statement.
  | Try
  -- | A catch statement.
  | Catch
  -- | A finally statement.
  | Finally
  -- | A class declaration.
  | Class
  -- | A class method declaration.
  | Method
  -- | A comment.
  | Comment
  -- | A non-standard category, which can be used for comparability.
  | Other Text
  -- | A relational operator (e.g. < or >=)
  | RelationalOperator
  -- | An empty statement. (e.g. ; in JavaScript)
  | Empty
  -- | A number literal.
  | NumberLiteral
  -- | A mathematical operator (e.g. +, -, *, /).
  | MathOperator
  -- | A module
  | Module
  -- | An import
  | Import
  -- | An export
  | Export
  -- | An interpolation (e.g. "#{bar}" in Ruby)
  | Interpolation
  -- | A subshell command (e.g. `ls -la` in Ruby)
  | Subshell
  -- | A conditional assignment expression.
  | ConditionalAssignment
  -- | A yield statement.
  | Yield
  -- | An until expression.
  | Until
  -- | A unless/else expression.
  | Unless
  | Begin
  | Else
  | Elsif
  | Ensure
  | Rescue
  | RescueModifier
  | When
  | RescuedException
  | Negate
  deriving (Eq, Generic, Ord, Show)

-- Instances

instance Hashable Category

instance Arbitrary Category where
  arbitrary = oneof [
      pure Program
    , pure Error
    , pure Boolean
    , pure BooleanOperator
    , pure MathOperator
    , pure DictionaryLiteral
    , pure Pair
    , pure FunctionCall
    , pure Function
    , pure Identifier
    , pure Params
    , pure ExpressionStatements
    , pure MethodCall
    , pure Args
    , pure StringLiteral
    , pure IntegerLiteral
    , pure NumberLiteral
    , pure Regex
    , pure Return
    , pure SymbolLiteral
    , pure TemplateString
    , pure ArrayLiteral
    , pure Assignment
    , pure MathAssignment
    , pure MemberAccess
    , pure SubscriptAccess
    , pure VarAssignment
    , pure VarDecl
    , pure For
    , pure DoWhile
    , pure While
    , pure Switch
    , pure Ternary
    , pure Case
    , pure Operator
    , pure Object
    , pure Throw
    , pure Constructor
    , pure Try
    , pure Catch
    , pure Finally
    , pure Class
    , pure Method
    , pure Module
    , pure Import
    , pure Export
    , pure Interpolation
    , pure Subshell
    , pure ConditionalAssignment
    , pure Yield
    , pure Until
    , pure Unless
    , pure Begin
    , pure Else
    , pure Elsif
    , pure Ensure
    , pure Rescue
    , pure RescueModifier
    , pure When
    , pure RescuedException
    , pure Negate
    , Other <$> arbitrary
    ]

  shrink (Other s) = Other <$> shrink s
  shrink _ = []
