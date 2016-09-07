{-# LANGUAGE DataKinds #-}
module Language.JavaScript where

import Data.Record
import Info
import Prologue
import Source
import SourceSpan
import qualified Syntax as S
import Term

termConstructor
  :: Source Char -- ^ The source that the term occurs within.
  -> IO SourceSpan -- ^ The span that the term occupies. This is passed in 'IO' to guarantee some access constraints & encourage its use only when needed (improving performance).
  -> Text -- ^ The name of the production for this node.
  -> Range -- ^ The character range that the term occupies.
  -> [Term Text (Record '[Range, Category])] -- ^ The child nodes of the term.
  -> IO (Term Text (Record '[Range, Category])) -- ^ The resulting term, in IO.
termConstructor source sourceSpan name range children
  | name == "ERROR" = sourceSpan >>= withDefaultInfo . (`S.Error` children)
  | otherwise = withDefaultInfo $ case (name, children) of
  ("return_statement", _) -> S.Return (listToMaybe children)
  ("assignment", [ identifier, value ]) -> S.Assignment identifier value
  ("math_assignment", [ identifier, value ]) -> S.MathAssignment identifier value
  ("member_access", [ base, property ]) -> S.MemberAccess base property
  ("subscript_access", [ base, element ]) -> S.SubscriptAccess base element
  ("comma_op", [ child, rest ]) -> S.Indexed $ child : toList (unwrap rest)
  _ | name `elem` [ "arrow_function", "generator_function", "function" ] -> case children of
    [ body ] -> S.AnonymousFunction Nothing body
    [ params, body ] -> S.AnonymousFunction (Just params) body
    [ id, params, body ] -> S.Function id (Just params) body
    _ -> S.Indexed children
  ("function_call", _) -> case runCofree <$> children of
    [ (_ :< S.MemberAccess{..}), (_ :< S.Args args) ] -> S.MethodCall memberId property args
    [ (_ :< S.MemberAccess{..}) ] -> S.MethodCall memberId property []
    (x:xs) -> S.FunctionCall (cofree x) (cofree <$> xs)
    _ -> S.Indexed children
  ("ternary", (condition:cases)) -> S.Ternary condition cases
  ("arguments", _) -> S.Args children
  ("var_assignment", [ x, y ]) -> S.VarAssignment x y
  ("var_declaration", _) -> S.Indexed $ toVarDecl <$> children
  ("switch_statement", (expr:rest)) -> S.Switch expr rest
  ("case", [ expr, body ]) -> S.Case expr body
  ("object", _) -> S.Object $ foldMap toTuple children
  ("pair", _) -> S.Fixed children
  ("if_statement", [ expr, clause1, clause2 ]) -> S.If expr clause1 (Just clause2)
  ("if_statement", [ expr, clause ]) -> S.If expr clause Nothing
  ("for_in_statement", _) | Just (exprs, body) <- unsnoc children -> S.For exprs body
  ("for_of_statement", _) | Just (exprs, body) <- unsnoc children -> S.For exprs body
  ("while_statement", [ expr, body ]) -> S.While expr body
  ("do_statement", [ expr, body ]) -> S.DoWhile expr body
  ("throw_statement", [ expr ]) -> S.Throw expr
  ("new_expression", [ expr ]) -> S.Constructor expr
  ("try_statement", [ body ]) -> S.Try body Nothing Nothing
  ("try_statement", [ body, catch ]) | Catch <- category (extract catch) -> S.Try body (Just catch) Nothing
  ("try_statement", [ body, finally ]) | Finally <- category (extract finally) -> S.Try body Nothing (Just finally)
  ("try_statement", [ body, catch, finally ])
    | Catch <- category (extract catch)
    , Finally <- category (extract finally) -> S.Try body (Just catch) (Just finally)
  ("array", _) -> S.Array children
  ("method_definition", [ identifier, params, exprs ]) -> S.Method identifier (toList (unwrap params)) (toList (unwrap exprs))
  ("method_definition", [ identifier, exprs ]) -> S.Method identifier [] (toList (unwrap exprs))
  ("class", [ identifier, superclass, definitions ]) -> S.Class identifier (Just superclass) (toList (unwrap definitions))
  ("class", [ identifier, definitions ]) -> S.Class identifier Nothing (toList (unwrap definitions))

  (_, []) -> S.Leaf . toText $ slice range source
  _ -> S.Indexed children
  where withDefaultInfo syntax@(S.MethodCall _ _ _) = pure $! cofree ((range .:  MethodCall .: RNil) :< syntax)
        withDefaultInfo syntax = pure $! cofree ((range .: categoryForJavaScriptProductionName name .: RNil) :< syntax)

categoryForJavaScriptProductionName :: Text -> Category
categoryForJavaScriptProductionName name = case name of
  "object" -> Object
  "expression_statement" -> ExpressionStatements
  "this_expression" -> Identifier
  "null" -> Identifier
  "undefined" -> Identifier
  "arrow_function" -> Function
  "generator_function" -> Function
  "math_op" -> BinaryOperator -- bitwise operator, e.g. +, -, *, /.
  "bool_op" -> BinaryOperator -- boolean operator, e.g. ||, &&.
  "comma_op" -> CommaOperator -- comma operator, e.g. expr1, expr2.
  "delete_op" -> Operator -- delete operator, e.g. delete x[2].
  "type_op" -> Operator -- type operator, e.g. typeof Object.
  "void_op" -> Operator -- void operator, e.g. void 2.
  "for_in_statement" -> For
  "for_of_statement" -> For
  "new_expression" -> Constructor
  "class"  -> Class
  "catch" -> Catch
  "finally" -> Finally
  "if_statement" -> If
  "empty_statement" -> Empty
  "program" -> Program
  "ERROR" -> Error
  "function_call" -> FunctionCall
  "pair" -> Pair
  "string" -> StringLiteral
  "integer" -> IntegerLiteral
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
  "switch_statement" -> Switch
  "math_assignment" -> MathAssignment
  "case" -> Case
  "true" -> Boolean
  "false" -> Boolean
  "ternary" -> Ternary
  "for_statement" -> For
  "while_statement" -> While
  "do_statement" -> DoWhile
  "return_statement" -> Return
  "throw_statement" -> Throw
  "try_statement" -> Try
  "method_definition" -> Method
  "comment" -> Comment
  "bitwise_op" -> BitwiseOperator
  "rel_op" -> RelationalOperator
  _ -> Other name

toVarDecl :: Term Text (Record fields) -> Term Text (Record fields)
toVarDecl child = cofree (extract child :< S.VarDecl child)

toTuple :: Term Text (Record fields) -> [Term Text (Record fields)]
toTuple child | S.Indexed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
toTuple child | S.Fixed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
toTuple child | S.Leaf c <- unwrap child = [cofree (extract child :< S.Comment c)]
toTuple child = pure child
