{-# LANGUAGE DataKinds #-}
module Language.JavaScript where

import Data.Record
import Info
import Prologue
import Source
import Language
import qualified Syntax as S
import Term

operators :: [Category]
operators = [ Operator, BooleanOperator, MathOperator, RelationalOperator, BitwiseOperator ]

termConstructor
  :: Source Char -- ^ The source that the term occurs within.
  -> SourceSpan -- ^ The span that the term occupies.
  -> Category -- ^ The node’s Category.
  -> Range -- ^ The character range that the term occupies.
  -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ The child nodes of the term.
  -> IO [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ All child nodes (included unnamed productions) of the term as 'IO'. Only use this if you need it.
  -> IO (SyntaxTerm Text '[Range, Category, SourceSpan]) -- ^ The resulting term, in IO.
termConstructor source sourceSpan category range children allChildren
  | category == Error = withDefaultInfo (S.Error children)
  | category `elem` operators = do
    allChildren' <- allChildren
    withDefaultInfo $ S.Operator allChildren'
  | otherwise = withDefaultInfo $ case (category, children) of
    (Return, _) -> S.Return children
    (Assignment, [ identifier, value ]) -> S.Assignment identifier value
    (Assignment, _ ) -> S.Error children
    (MathAssignment, [ identifier, value ]) -> S.OperatorAssignment identifier value
    (MathAssignment, _ ) -> S.Error children
    (MemberAccess, [ base, property ]) -> S.MemberAccess base property
    (MemberAccess, _ ) -> S.Error children
    (SubscriptAccess, [ base, element ]) -> S.SubscriptAccess base element
    (SubscriptAccess, _ ) -> S.Error children
    (CommaOperator, [ a, b ]) -> case unwrap b of
      S.Indexed rest -> S.Indexed $ a : rest
      _ -> S.Indexed children
    (CommaOperator, _ ) -> S.Error children
    (FunctionCall, _) -> case children of
      member : args | Info.category (extract member) == MemberAccess -> case toList (unwrap member) of
        [target, method] -> S.MethodCall target method (toList . unwrap =<< args)
        _ -> S.Error children
      function : args -> S.FunctionCall function (toList . unwrap =<< args)
      _ -> S.Error children
    (Ternary, condition : cases) -> S.Ternary condition cases
    (Ternary, _ ) -> S.Error children
    (VarAssignment, [ x, y ]) -> S.VarAssignment x y
    (VarAssignment, _ ) -> S.Error children
    (VarDecl, _) -> S.Indexed $ toVarDecl <$> children
    (Switch, expr : rest) -> S.Switch (Just expr) rest
    (Switch, _ ) -> S.Error children
    (Case, [ expr, body ]) -> S.Case expr [body]
    (Case, _ ) -> S.Error children
    (Object, _) -> S.Object Nothing $ foldMap toTuple children
    (Pair, _) -> S.Fixed children
    (Comment, _) -> S.Comment . toText $ slice range source
    (If, expr : rest ) -> S.If expr rest
    (If, _ ) -> S.Error children
    (While, expr : rest ) -> S.While expr rest
    (While, _ ) -> S.Error children
    (DoWhile, [ expr, body ]) -> S.DoWhile expr body
    (DoWhile, _ ) -> S.Error children
    (Throw, [ expr ]) -> S.Throw expr
    (Throw, _ ) -> S.Error children
    (Constructor, [ expr ]) -> S.Constructor expr
    (Constructor, _ ) -> S.Error children
    (Try, _) -> case children of
      [ body ] -> S.Try [body] [] Nothing Nothing
      [ body, catch ] | Catch <- Info.category (extract catch) -> S.Try [body] [catch] Nothing Nothing
      [ body, finally ] | Finally <- Info.category (extract finally) -> S.Try [body] [] Nothing (Just finally)
      [ body, catch, finally ]
        | Catch <- Info.category (extract catch)
        , Finally <- Info.category (extract finally) -> S.Try [body] [catch] Nothing (Just finally)
      _ -> S.Error children
    (ArrayLiteral, _) -> S.Array Nothing children
    (Method, [ identifier, params, exprs ]) -> S.Method identifier Nothing (toList (unwrap params)) (toList (unwrap exprs))
    (Method, [ identifier, exprs ]) -> S.Method identifier Nothing [] (toList (unwrap exprs))
    (Method, _ ) -> S.Error children
    (Class, [ identifier, superclass, definitions ]) -> S.Class identifier (Just superclass) (toList (unwrap definitions))
    (Class, [ identifier, definitions ]) -> S.Class identifier Nothing (toList (unwrap definitions))
    (Class, _ ) -> S.Error children
    (Import, [ statements, identifier ] ) -> S.Import identifier (toList (unwrap statements))
    (Import, [ identifier ] ) -> S.Import identifier []
    (Import, _ ) -> S.Error children
    (Export, [ statements, identifier] ) -> S.Export (Just identifier) (toList (unwrap statements))
    (Export, [ statements ] ) -> case unwrap statements of
      S.Indexed _ -> S.Export Nothing (toList (unwrap statements))
      _ -> S.Export (Just statements) []
    (Export, _ ) -> S.Error children
    (Break, [ expr ] ) -> S.Break (Just expr)
    (Yield, _ ) -> S.Yield children
    (For, _) -> case unsnoc children of
      Just (exprs, body) -> S.For exprs [body]
      _ -> S.Error children
    (Function, _) -> case children of
      [ body ] -> S.AnonymousFunction [] [body]
      [ params, body ] -> S.AnonymousFunction (toList (unwrap params)) [body]
      [ id, params, body ] -> S.Function id (toList (unwrap params)) Nothing [body]
      _ -> S.Error children
    (_, []) -> S.Leaf . toText $ slice range source
    _ -> S.Indexed children
  where
    withDefaultInfo syntax =
      pure $! case syntax of
        S.MethodCall{} -> cofree ((range :.  MethodCall :. sourceSpan :. Nil) :< syntax)
        _ -> cofree ((range :. category :. sourceSpan :. Nil) :< syntax)

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
  "ERROR" -> Error
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
