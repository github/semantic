{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Category where

import Prologue
import Data.Functor.Listable
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)

-- | A standardized category of AST node. Used to determine the semantics for
-- | semantic diffing and define comparability of nodes.
data Category
  -- | The top-level branch node.
  = Program
  -- | A node indicating syntax errors.
  | ParseError
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
  -- | Formerly used for Ruby’s @x rescue y@ modifier syntax. Deprecated. Use @Modifier Rescue@ instead. Left in place to preserve hashing & RWS results.
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
  -- | A float literal.
  | FloatLiteral
  -- | An array type declaration, e.g. [2]string in Go.
  | ArrayTy
  -- | A dictionary type declaration, e.g. map[string] in Go.
  | DictionaryTy
  -- | A Struct type declaration, struct Foo {..} in Go.
  | StructTy
  -- | A Struct constructor, e.g. foo = Foo {..} in Go.
  | Struct
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
  | ParameterDecl
  -- | A default case in a switch statement.
  | DefaultCase
  -- | A type declaration.
  | TypeDecl
  | PointerTy
  -- | A field declaration.
  | FieldDecl
  -- | A slice type, e.g. []string{"hello"} in Go.
  | SliceTy
  -- | An element of a slice literal.
  | Element
  -- | A literal value.
  | Literal
  -- | A channel type in Go.
  | ChannelTy
  -- | A send statement in Go.
  | Send
  -- | An Index expression, e.g. x[1] in Go.
  | IndexExpression
  -- | A function type.
  | FunctionTy
  -- | An increment statement, e.g. i++ in Go.
  | IncrementStatement
  -- | A decrement statement, e.g. i-- in Go.
  | DecrementStatement
  -- | A qualified identifier, e.g. Module.function in Go.
  | QualifiedIdentifier
  | FieldDeclarations
  -- | A Go rune literal.
  | RuneLiteral
  -- | A modifier version of another Category, e.g. Ruby’s trailing @if@, @while@, etc. terms, whose subterms are swapped relative to regular @if@, @while@, etc. terms.
  | Modifier Category
  -- | A singleton method declaration, e.g. `def self.foo;end` in Ruby
  | SingletonMethod
  deriving (Eq, Generic, Ord, Show)

{-# DEPRECATED RescueModifier "Deprecated; use Modifier Rescue instead." #-}


-- Instances

instance Hashable Category

instance (StringConv Category Text) where
  strConv _ = pack . show

instance (StringConv Category ByteString) where
  strConv _ = encodeUtf8 . show

instance Listable Category where
  tiers = cons0 Program
       \/ cons0 ParseError
       \/ cons0 Boolean
       \/ cons0 BooleanOperator
      --  \/ cons0 MathOperator
      --  \/ cons0 DictionaryLiteral
      --  \/ cons0 Pair
       \/ cons0 FunctionCall
       \/ cons0 Function
       \/ cons0 Identifier
      --  \/ cons0 Params
      --  \/ cons0 ExpressionStatements
       \/ cons0 MethodCall
      --  \/ cons0 Args
       \/ cons0 StringLiteral
       \/ cons0 IntegerLiteral
       \/ cons0 NumberLiteral
      --  \/ cons0 Regex
       \/ cons0 Return
      --  \/ cons0 SymbolLiteral
      --  \/ cons0 TemplateString
      --  \/ cons0 ArrayLiteral
      --  \/ cons0 Assignment
      --  \/ cons0 MathAssignment
      --  \/ cons0 MemberAccess
      --  \/ cons0 SubscriptAccess
      --  \/ cons0 VarAssignment
      --  \/ cons0 VarDecl
      --  \/ cons0 For
      --  \/ cons0 DoWhile
      --  \/ cons0 While
      --  \/ cons0 Switch
       \/ cons0 If
      --  \/ cons0 Ternary
      --  \/ cons0 Case
      --  \/ cons0 Operator
      --  \/ cons0 CommaOperator
      --  \/ cons0 Object
      --  \/ cons0 Throw
      --  \/ cons0 Constructor
      --  \/ cons0 Try
      --  \/ cons0 Catch
      --  \/ cons0 Finally
       \/ cons0 Class
       \/ cons0 Method
      --  \/ cons0 Comment
      --  \/ cons0 RelationalOperator
      --  \/ cons0 Empty
      --  \/ cons0 Module
      --  \/ cons0 Import
      --  \/ cons0 Export
      --  \/ cons0 AnonymousFunction
      --  \/ cons0 Interpolation
      --  \/ cons0 Subshell
      --  \/ cons0 OperatorAssignment
      --  \/ cons0 Yield
      --  \/ cons0 Until
      --  \/ cons0 Unless
      --  \/ cons0 Begin
      --  \/ cons0 Else
      --  \/ cons0 Elsif
      --  \/ cons0 Ensure
      --  \/ cons0 Rescue
      --  \/ cons0 RescueModifier
      --  \/ cons0 RescuedException
      --  \/ cons0 RescueArgs
      --  \/ cons0 When
      --  \/ cons0 Negate
      --  \/ cons0 Select
      --  \/ cons0 Defer
      --  \/ cons0 Go
      --  \/ cons0 Slice
      --  \/ cons0 TypeAssertion
      --  \/ cons0 TypeConversion
      --  \/ cons0 ArgumentPair
      --  \/ cons0 KeywordParameter
      --  \/ cons0 OptionalParameter
      --  \/ cons0 SplatParameter
      --  \/ cons0 HashSplatParameter
      --  \/ cons0 BlockParameter
      --  \/ cons0 FloatLiteral
      --  \/ cons0 ArrayTy
      --  \/ cons0 DictionaryTy
      --  \/ cons0 StructTy
      --  \/ cons0 Struct
      --  \/ cons0 Break
      --  \/ cons0 Continue
       \/ cons0 Binary
       \/ cons0 Unary
      --  \/ cons0 Constant
      --  \/ cons0 Superclass
      --  \/ cons0 SingletonClass
      --  \/ cons0 RangeExpression
      --  \/ cons0 ScopeOperator
      --  \/ cons0 BeginBlock
      --  \/ cons0 EndBlock
      --  \/ cons0 ParameterDecl
      --  \/ cons0 DefaultCase
      --  \/ cons0 TypeDecl
      --  \/ cons0 PointerTy
      --  \/ cons0 FieldDecl
      --  \/ cons0 SliceTy
      --  \/ cons0 Element
      --  \/ cons0 Literal
      --  \/ cons0 ChannelTy
      --  \/ cons0 Send
      --  \/ cons0 IndexExpression
      --  \/ cons0 FunctionTy
      --  \/ cons0 IncrementStatement
      --  \/ cons0 DecrementStatement
      --  \/ cons0 QualifiedIdentifier
      --  \/ cons0 FieldDeclarations
      --  \/ cons0 RuneLiteral
      --  \/ cons0 (Modifier If)
       \/ cons0 SingletonMethod
      --  \/ cons0 (Other "other")
