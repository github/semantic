{-# LANGUAGE DataKinds, GADTs #-}
module Language.JavaScript where

import Data.Record
import Info
import Prologue
import Source
import Language
import qualified Syntax as S
import Term

termAssignment
  :: Source Char -- ^ The source of the term.
  -> Record '[Range, Category, SourceSpan] -- ^ The proposed annotation for the term.
  -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ The child nodes of the term.
  -> Maybe (S.Syntax Text (SyntaxTerm Text '[Range, Category, SourceSpan])) -- ^ The resulting term, in IO.
termAssignment source (_ :. category :. _ :. Nil) children
  = Just $ case (category, children) of
    (Return, _) -> S.Return children
    (Assignment, [ identifier, value ]) -> S.Assignment identifier value
    (MathAssignment, [ identifier, value ]) -> S.OperatorAssignment identifier value
    (MemberAccess, [ base, property ]) -> S.MemberAccess base property
    (SubscriptAccess, [ base, element ]) -> S.SubscriptAccess base element
    (CommaOperator, [ a, b ]) -> case unwrap b of
      S.Indexed rest -> S.Indexed $ a : rest
      _ -> S.Indexed children
    (FunctionCall, fn : args) | MemberAccess <- Info.category (extract fn)
                              , [target, method] <- toList (unwrap fn)
                              -> S.MethodCall target method (toList . unwrap =<< args)
                              | otherwise
                              -> S.FunctionCall fn (toList . unwrap =<< args)
    (Ternary, condition : cases) -> S.Ternary condition cases
    (VarAssignment, [ x, y ]) -> S.VarAssignment x y
    (VarDecl, _) -> S.Indexed $ toVarDecl <$> children
    (Switch, expr : rest) -> S.Switch (Just expr) rest
    (Case, [ expr, body ]) -> S.Case expr [body]
    (Object, _) -> S.Object Nothing $ foldMap toTuple children
    (Pair, _) -> S.Fixed children
    (Comment, _) -> S.Comment $ toText source
    (If, expr : rest ) -> S.If expr rest
    (While, expr : rest ) -> S.While expr rest
    (DoWhile, [ expr, body ]) -> S.DoWhile expr body
    (Throw, [ expr ]) -> S.Throw expr
    (Constructor, [ expr ]) -> S.Constructor expr
    (Try, body : rest) | null rest
                       -> S.Try [body] [] Nothing Nothing
                       | [catch] <- rest
                       , Catch <- Info.category (extract catch)
                       -> S.Try [body] [catch] Nothing Nothing
                       | [finally] <- rest
                       , Finally <- Info.category (extract finally)
                       -> S.Try [body] [] Nothing (Just finally)
                       | [ catch, finally ] <- rest
                      , Catch <- Info.category (extract catch)
                      , Finally <- Info.category (extract finally)
                      -> S.Try [body] [catch] Nothing (Just finally)
    (ArrayLiteral, _) -> S.Array Nothing children
    (Method, [ identifier, params, exprs ]) -> S.Method identifier Nothing (toList (unwrap params)) (toList (unwrap exprs))
    (Method, [ identifier, exprs ]) -> S.Method identifier Nothing [] (toList (unwrap exprs))
    (Class, [ identifier, superclass, definitions ]) -> S.Class identifier (Just superclass) (toList (unwrap definitions))
    (Class, [ identifier, definitions ]) -> S.Class identifier Nothing (toList (unwrap definitions))
    (Import, [ statements, identifier ] ) -> S.Import identifier (toList (unwrap statements))
    (Import, [ identifier ] ) -> S.Import identifier []
    (Export, [ statements, identifier] ) -> S.Export (Just identifier) (toList (unwrap statements))
    (Export, [ statements ] ) -> case unwrap statements of
      S.Indexed _ -> S.Export Nothing (toList (unwrap statements))
      _ -> S.Export (Just statements) []
    (Break, [ expr ] ) -> S.Break (Just expr)
    (Yield, _ ) -> S.Yield children
    (For, _) -> case unsnoc children of
      Just (exprs, body) -> S.For exprs [body]
      _ -> S.Error children
    (Function, _) -> case children of
      [ body ] -> S.AnonymousFunction [] [body]
      [ params, body ] -> S.AnonymousFunction (toList (unwrap params)) [body]
      [ id, params, body ] -> S.Function id (toList (unwrap params)) [body]
      _ -> S.Error children
    (_, []) -> S.Leaf $ toText source
    _ -> S.Indexed children

categoryForJavaScriptProductionName :: Text -> Category
categoryForJavaScriptProductionName name = case name of
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
  "var_assignment" -> VarAssignment
  "var_declaration" -> VarDecl
  "trailing_var_declaration" -> VarDecl
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
  "yield_statement" -> Yield
  _ -> Other name
