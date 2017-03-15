{-# LANGUAGE DataKinds #-}
module Language.TypeScript where

import Info
import Prologue
import Source
import Language
import qualified Syntax as S
import Term

termAssignment
  :: Source -- ^ The source of the term.
  -> Category -- ^ The category for the term.
  -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ The child nodes of the term.
  -> Maybe (S.Syntax Text (SyntaxTerm Text '[Range, Category, SourceSpan])) -- ^ The resulting term, in Maybe.
termAssignment _ category children = undefined

categoryForTypeScriptName :: Text -> Category
categoryForTypeScriptName = \case
  "object" -> Object
  "expression_statement" -> ExpressionStatements
  "trailing_expression_statement" -> ExpressionStatements
  "this_expression" -> Identifier
  "null" -> Identifier
  "undefined" -> Identifier
  "arrow_function" -> Function
  "generator_function" -> Function
  "math_op" -> MathOperator -- math operator, e.g. +, -, *, /.
  "bool_op" -> BooleanOperator -- boolean operator, e.g. ||, &&.
  "comma_op" -> CommaOperator -- comma operator, e.g. expr1, expr2.
  "delete_op" -> Operator -- delete operator, e.g. delete x[2].
  "type_op" -> Operator -- type operator, e.g. typeof Object.
  "void_op" -> Operator -- void operator, e.g. void 2.
  "for_statement" -> For
  "trailing_for_statement" -> For
  "for_in_statement" -> For
  "trailing_for_in_statement" -> For
  "for_of_statement" -> For
  "trailing_for_of_statement" -> For
  "new_expression" -> Constructor
  "class"  -> Class
  "catch" -> Catch
  "finally" -> Finally
  "if_statement" -> If
  "trailing_if_statement" -> If
  "empty_statement" -> Empty
  "program" -> Program
  "function_call" -> FunctionCall
  "pair" -> Pair
  "string" -> StringLiteral
  "integer" -> IntegerLiteral
  "number" -> NumberLiteral
  "float" -> FloatLiteral
  "symbol" -> SymbolLiteral
  "array" -> ArrayLiteral
  "function" -> Function
  "identifier" -> Identifier
  "formal_parameters" -> Params
  "arguments" -> Args
  "statement_block" -> ExpressionStatements
  "assignment" -> Assignment
  "member_access" -> MemberAccess
  "op" -> Operator
  "subscript_access" -> SubscriptAccess
  "regex" -> Regex
  "template_string" -> TemplateString
  "variable_declaration" -> VarDecl
  "trailing_variable_declaration" -> VarDecl
  "switch_statement" -> Switch
  "math_assignment" -> MathAssignment
  "case" -> Case
  "true" -> Boolean
  "false" -> Boolean
  "ternary" -> Ternary
  "while_statement" -> While
  "trailing_while_statement" -> While
  "do_statement" -> DoWhile
  "trailing_do_statement" -> DoWhile
  "return_statement" -> Return
  "trailing_return_statement" -> Return
  "throw_statement" -> Throw
  "trailing_throw_statement" -> Throw
  "try_statement" -> Try
  "method_definition" -> Method
  "comment" -> Comment
  "bitwise_op" -> BitwiseOperator
  "rel_op" -> RelationalOperator
  "import_statement" -> Import
  "export_statement" -> Export
  "break_statement" -> Break
  "continue_statement" -> Continue
  "yield_expression" -> Yield
  _ -> Other name
