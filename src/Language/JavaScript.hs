{-# LANGUAGE DataKinds #-}
module Language.JavaScript where

import Data.Record
import Info
import Prologue
import Source
import Language
import qualified Syntax as S
import Term

operators :: [Text]
operators = [ "op", "bool_op", "math_op", "delete_op", "type_op", "void_op", "rel_op", "bitwise_op" ]

functions :: [Text]
functions = [ "arrow_function", "generator_function", "function" ]

forStatements :: [Text]
forStatements = [ "for_statement", "for_of_statement", "for_in_statement", "trailing_for_statement", "trailing_for_of_statement", "trailing_for_in_statement" ]

termConstructor
  :: Source Char -- ^ The source that the term occurs within.
  -> IO SourceSpan -- ^ The span that the term occupies. This is passed in 'IO' to guarantee some access constraints & encourage its use only when needed (improving performance).
  -> Text -- ^ The name of the production for this node.
  -> Range -- ^ The character range that the term occupies.
  -> [Term (S.Syntax Text) (Record '[Range, Category, SourceSpan])] -- ^ The child nodes of the term.
  -> IO (Term (S.Syntax Text) (Record '[Range, Category, SourceSpan])) -- ^ The resulting term, in IO.
termConstructor source sourceSpan name range children
  | name == "ERROR" = withDefaultInfo (S.Error children)
  | otherwise = withDefaultInfo $ case (name, children) of
    ("return_statement", _) -> S.Return (listToMaybe children)
    ("trailing_return_statement", _) -> S.Return (listToMaybe children)
    ("assignment", [ identifier, value ]) -> S.Assignment identifier value
    ("assignment", _ ) -> S.Error children
    ("math_assignment", [ identifier, value ]) -> S.MathAssignment identifier value
    ("math_assignment", _ ) -> S.Error children
    ("member_access", [ base, property ]) -> S.MemberAccess base property
    ("member_access", _ ) -> S.Error children
    ("subscript_access", [ base, element ]) -> S.SubscriptAccess base element
    ("subscript_access", _ ) -> S.Error children
    ("comma_op", [ a, b ]) -> case unwrap b of
      S.Indexed rest -> S.Indexed $ a : rest
      _ -> S.Indexed children
    ("comma_op", _ ) -> S.Error children
    ("function_call", _) -> case children of
      member : args | category (extract member) == MemberAccess -> case toList (unwrap member) of
        [target, method] -> S.MethodCall target method (toList . unwrap =<< args)
        _ -> S.Error children
      function : args -> S.FunctionCall function (toList . unwrap =<< args)
      _ -> S.Error children
    ("ternary", condition : cases) -> S.Ternary condition cases
    ("ternary", _ ) -> S.Error children
    ("var_assignment", [ x, y ]) -> S.VarAssignment x y
    ("var_assignment", _ ) -> S.Error children
    ("var_declaration", _) -> S.Indexed $ toVarDecl <$> children
    ("trailing_var_declaration", _) -> S.Indexed $ toVarDecl <$> children
    ("switch_statement", expr : rest) -> S.Switch expr rest
    ("switch_statement", _ ) -> S.Error children
    ("case", [ expr, body ]) -> S.Case expr [body]
    ("case", _ ) -> S.Error children
    ("object", _) -> S.Object $ foldMap toTuple children
    ("pair", _) -> S.Fixed children
    ("comment", _) -> S.Comment . toText $ slice range source
    ("if_statement", expr : rest ) -> S.If expr rest
    ("trailing_if_statement", expr : rest ) -> S.If expr rest
    ("if_statement", _ ) -> S.Error children
    ("trailing_if_statement", _ ) -> S.Error children
    ("while_statement", expr : rest ) -> S.While expr rest
    ("trailing_while_statement", expr : rest ) -> S.While expr rest
    ("while_statement", _ ) -> S.Error children
    ("trailing_while_statement", _ ) -> S.Error children
    ("do_statement", [ expr, body ]) -> S.DoWhile expr body
    ("trailing_do_statement", [ expr, body ]) -> S.DoWhile expr body
    ("do_statement", _ ) -> S.Error children
    ("trailing_do_statement", _ ) -> S.Error children
    ("throw_statement", [ expr ]) -> S.Throw expr
    ("trailing_throw_statement", [ expr ]) -> S.Throw expr
    ("throw_statment", _ ) -> S.Error children
    ("trailing_throw_statment", _ ) -> S.Error children
    ("new_expression", [ expr ]) -> S.Constructor expr
    ("new_expression", _ ) -> S.Error children
    ("try_statement", _) -> case children of
      [ body ] -> S.Try [body] [] Nothing Nothing
      [ body, catch ] | Catch <- category (extract catch) -> S.Try [body] [catch] Nothing Nothing
      [ body, finally ] | Finally <- category (extract finally) -> S.Try [body] [] Nothing (Just finally)
      [ body, catch, finally ]
        | Catch <- category (extract catch)
        , Finally <- category (extract finally) -> S.Try [body] [catch] Nothing (Just finally)
      _ -> S.Error children
    ("array", _) -> S.Array children
    ("method_definition", [ identifier, params, exprs ]) -> S.Method identifier (toList (unwrap params)) (toList (unwrap exprs))
    ("method_definition", [ identifier, exprs ]) -> S.Method identifier [] (toList (unwrap exprs))
    ("method_definition", _ ) -> S.Error children
    ("class", [ identifier, superclass, definitions ]) -> S.Class identifier (Just superclass) (toList (unwrap definitions))
    ("class", [ identifier, definitions ]) -> S.Class identifier Nothing (toList (unwrap definitions))
    ("class", _ ) -> S.Error children
    ("import_statement", [ statements, identifier ] ) -> S.Import identifier (toList (unwrap statements))
    ("import_statement", [ identifier ] ) -> S.Import identifier []
    ("import_statement", _ ) -> S.Error children
    ("export_statement", [ statements, identifier] ) -> S.Export (Just identifier) (toList (unwrap statements))
    ("export_statement", [ statements ] ) -> case unwrap statements of
      S.Indexed _ -> S.Export Nothing (toList (unwrap statements))
      _ -> S.Export (Just statements) []
    ("export_statement", _ ) -> S.Error children
    ("break_statement", [ expr ] ) -> S.Break expr
    _ | name `elem` forStatements -> case unsnoc children of
          Just (exprs, body) -> S.For exprs [body]
          _ -> S.Error children
    _ | name `elem` operators -> S.Operator children
    _ | name `elem` functions -> case children of
          [ body ] -> S.AnonymousFunction [] [body]
          [ params, body ] -> S.AnonymousFunction (toList (unwrap params)) [body]
          [ id, params, body ] -> S.Function id (toList (unwrap params)) [body]
          _ -> S.Error children
    (_, []) -> S.Leaf . toText $ slice range source
    _ -> S.Indexed children
  where
    withDefaultInfo syntax = do
      sourceSpan' <- sourceSpan
      pure $! case syntax of
        S.MethodCall{} -> cofree ((range .:  MethodCall .: sourceSpan' .: RNil) :< syntax)
        _ -> cofree ((range .: categoryForJavaScriptProductionName name .: sourceSpan' .: RNil) :< syntax)

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
  _ -> Other name
