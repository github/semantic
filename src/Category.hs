{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Category
( Category(..)
) where

import Prologue
import Test.QuickCheck hiding (Args)
import Data.Functor.Listable
import Data.Text (pack)
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
  -- | An anonymous function.
  | AnonymousFunction
  -- | An interpolation (e.g. "#{bar}" in Ruby)
  | Interpolation
  -- | A subshell command (e.g. `ls -la` in Ruby)
  | Subshell
  -- | Operator assignment, e.g. a ||= b, a += 1 in Ruby.
  | OperatorAssignment
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
  | RescuedException
  | RescueArgs
  | When
  | Negate
  -- | A select expression in Go.
  | Select
  | Defer
  | Go
  | Slice
  | TypeAssertion
  | TypeConversion
  -- | An argument pair, e.g. foo(run: true) or foo(:run => true) in Ruby.
  | ArgumentPair
  -- | A keyword parameter, e.g. def foo(name:) or def foo(name:false) in Ruby.
  | KeywordParameter
  -- | An optional/default parameter, e.g. def foo(name = nil) in Ruby.
  | OptionalParameter
  -- | A splat parameter, e.g. def foo(*array) in Ruby.
  | SplatParameter
  -- | A hash splat parameter, e.g. def foo(**option) in Ruby.
  | HashSplatParameter
  -- | A block parameter, e.g. def foo(&block) in Ruby.
  | BlockParameter
  -- | A break statement, e.g. break; in JavaScript.
  | Break
  -- | A continue statement, e.g. continue; in JavaScript.
  | Continue
  -- | A binary statement, e.g. a | b in Ruby.
  | Binary
  -- | A unary statement, e.g. !a in Ruby.
  | Unary
  -- | A constant, e.g `Foo::Bar` in Ruby.
  | Constant
  -- | A superclass, e.g `< Foo` in Ruby.
  | Superclass
  -- | A singleton class declaration, e.g. `class << self;end` in Ruby
  | SingletonClass
  -- | A range expression, e.g. `1..10` in Ruby.
  | RangeExpression
  -- | A scope resolution operator, e.g. `Foo::bar` in Ruby.
  | ScopeOperator
  -- | A BEGIN {} block of statements.
  | BeginBlock
  -- | An END {} block of statements.
  | EndBlock
  deriving (Eq, Generic, Ord, Show)

-- Instances

instance Hashable Category

instance (StringConv Category Text) where
  strConv _ = pack . show

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
    , pure OperatorAssignment
    , pure Yield
    , pure Until
    , pure Unless
    , pure Begin
    , pure Else
    , pure Elsif
    , pure Ensure
    , pure Rescue
    , pure RescueModifier
    , pure RescuedException
    , pure RescueArgs
    , pure When
    , pure Negate
    , pure Select
    , pure Defer
    , pure Go
    , pure Slice
    , pure TypeAssertion
    , pure TypeConversion
    , pure ArgumentPair
    , pure KeywordParameter
    , pure OptionalParameter
    , pure SplatParameter
    , pure HashSplatParameter
    , pure BlockParameter
    , pure Break
    , pure Continue
    , pure Binary
    , pure Unary
    , pure Constant
    , pure Superclass
    , pure SingletonClass
    , Other <$> arbitrary
    ]

  shrink (Other s) = Other <$> shrink s
  shrink _ = []

instance Listable Category where
  tiers
    =  cons0 Program
    \/ cons0 Error
    \/ cons0 Boolean
    \/ cons0 BooleanOperator
    \/ cons0 MathOperator
    \/ cons0 DictionaryLiteral
    \/ cons0 Pair
    \/ cons0 FunctionCall
    \/ cons0 Function
    \/ cons0 Identifier
    \/ cons0 Params
    \/ cons0 ExpressionStatements
    \/ cons0 MethodCall
    \/ cons0 Args
    \/ cons0 StringLiteral
    \/ cons0 IntegerLiteral
    \/ cons0 NumberLiteral
    \/ cons0 Regex
    \/ cons0 Return
    \/ cons0 SymbolLiteral
    \/ cons0 TemplateString
    \/ cons0 ArrayLiteral
    \/ cons0 Assignment
    \/ cons0 MathAssignment
    \/ cons0 MemberAccess
    \/ cons0 SubscriptAccess
    \/ cons0 VarAssignment
    \/ cons0 VarDecl
    \/ cons0 For
    \/ cons0 DoWhile
    \/ cons0 While
    \/ cons0 Switch
    \/ cons0 Ternary
    \/ cons0 Case
    \/ cons0 Operator
    \/ cons0 Object
    \/ cons0 Throw
    \/ cons0 Constructor
    \/ cons0 Try
    \/ cons0 Catch
    \/ cons0 Finally
    \/ cons0 Class
    \/ cons0 Method
    \/ cons0 Module
    \/ cons0 Import
    \/ cons0 Export
    \/ cons0 Interpolation
    \/ cons0 Subshell
    \/ cons0 OperatorAssignment
    \/ cons0 Yield
    \/ cons0 Until
    \/ cons0 Unless
    \/ cons0 Begin
    \/ cons0 Else
    \/ cons0 Elsif
    \/ cons0 Ensure
    \/ cons0 Rescue
    \/ cons0 RescueModifier
    \/ cons0 RescuedException
    \/ cons0 RescueArgs
    \/ cons0 When
    \/ cons0 Negate
    \/ cons0 Select
    \/ cons0 Defer
    \/ cons0 Go
    \/ cons0 Slice
    \/ cons0 TypeAssertion
    \/ cons0 TypeConversion
    \/ cons0 ArgumentPair
    \/ cons0 KeywordParameter
    \/ cons0 OptionalParameter
    \/ cons0 SplatParameter
    \/ cons0 HashSplatParameter
    \/ cons0 BlockParameter
    \/ cons0 Break
    \/ cons0 Continue
    \/ cons0 Binary
    \/ cons0 Unary
    \/ cons0 Constant
    \/ cons0 Superclass
    \/ cons0 SingletonClass
    \/ cons1 (Other . unListableText)

newtype ListableText = ListableText { unListableText :: Text }

instance Listable ListableText where
  tiers = cons1 (ListableText . pack)
