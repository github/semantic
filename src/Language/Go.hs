{-# LANGUAGE DataKinds #-}
module Language.Go where

import Data.Source
import Info
import Prologue
import qualified Syntax as S
import Term

termAssignment
  :: Source -- ^ The source of the term.
  -> Category -- ^ The category for the term.
  -> [ SyntaxTerm DefaultFields ] -- ^ The child nodes of the term.
  -> Maybe (S.Syntax (SyntaxTerm DefaultFields)) -- ^ The resulting term, in Maybe.
termAssignment source category children = case (category, children) of
  (Module, [moduleName]) -> Just $ S.Module moduleName []
  (Import, [importName]) -> Just $ S.Import importName []
  (Function, [id, params, block]) -> Just $ S.Function id [params] (toList (unwrap block))
  (Function, [id, params, ty, block]) -> Just $ S.Function id [params, ty] (toList (unwrap block))
  (For, [body]) | Other "block" <- Info.category (extract body) -> Just $ S.For [] (toList (unwrap body))
  (For, [forClause, body]) | Other "for_clause" <- Info.category (extract forClause) -> Just $ S.For (toList (unwrap forClause)) (toList (unwrap body))
  (For, [rangeClause, body]) | Other "range_clause" <- Info.category (extract rangeClause) -> Just $ S.For (toList (unwrap rangeClause)) (toList (unwrap body))
  (TypeDecl, [identifier, ty]) -> Just $ S.TypeDecl identifier ty
  (StructTy, _) -> Just (S.Ty children)
  (FieldDecl, _) -> Just (S.FieldDecl children)
  (ParameterDecl, param : ty) -> Just $ S.ParameterDecl (listToMaybe ty) param
  (Assignment, [identifier, expression]) -> Just $ S.VarAssignment [identifier] expression
  (Select, _) -> Just $ S.Select (children >>= toList . unwrap)
  (Go, [expr]) -> Just $ S.Go expr
  (Defer, [expr]) -> Just $ S.Defer expr
  (SubscriptAccess, [a, b]) -> Just $ S.SubscriptAccess a b
  (IndexExpression, [a, b]) -> Just $ S.SubscriptAccess a b
  (Slice, [a, rest]) -> Just $ S.SubscriptAccess a rest
  (Literal, children) -> Just . S.Indexed $ unpackElement <$> children
  (Other "composite_literal", [ty, values])
    | ArrayTy <- Info.category (extract ty)
    -> Just $ S.Array (Just ty) (toList (unwrap values))
    | DictionaryTy <- Info.category (extract ty)
    -> Just $ S.Object (Just ty) (toList (unwrap values))
    | SliceTy <- Info.category (extract ty)
    -> Just $ S.SubscriptAccess ty values
  (Other "composite_literal", []) -> Just $ S.Struct Nothing []
  (Other "composite_literal", [ty]) -> Just $ S.Struct (Just ty) []
  (Other "composite_literal", [ty, values]) -> Just $ S.Struct (Just ty) (toList (unwrap values))
  (TypeAssertion, [a, b]) -> Just $ S.TypeAssertion a b
  (TypeConversion, [a, b]) -> Just $ S.TypeConversion a b
  -- TODO: Handle multiple var specs
  (VarAssignment, [identifier, expression]) -> Just $ S.VarAssignment [identifier] expression
  (VarDecl, children) -> Just $ S.VarDecl children
  (FunctionCall, id : rest) -> Just $ S.FunctionCall id [] rest
  (AnonymousFunction, [params, _, body])
    | [params'] <- toList (unwrap params)
    -> Just $ S.AnonymousFunction (toList (unwrap params')) (toList (unwrap body))
  (PointerTy, _) -> Just $ S.Ty children
  (ChannelTy, _) -> Just $ S.Ty children
  (Send, [channel, expr]) -> Just $ S.Send channel expr
  (Operator, _) -> Just $ S.Operator children
  (FunctionTy, _) -> Just $ S.Ty children
  (IncrementStatement, _) -> Just $ S.Leaf (toText source)
  (DecrementStatement, _) -> Just $ S.Leaf (toText source)
  (QualifiedIdentifier, _) -> Just $ S.Leaf (toText source)
  (Method, [receiverParams, name, body]) -> Just (S.Method [] name (Just receiverParams) [] (toList (unwrap body)))
  (Method, [receiverParams, name, params, body])
    -> Just (S.Method [] name (Just receiverParams) [params] (toList (unwrap body)))
  (Method, [receiverParams, name, params, ty, body])
    -> Just (S.Method [] name (Just receiverParams) [params, ty] (toList (unwrap body)))
  _ -> Nothing
  where unpackElement element
          | Element <- Info.category (extract element)
          , S.Indexed [ child ] <- unwrap element = child
          | otherwise                             = element

categoryForGoName :: Text -> Category
categoryForGoName name = case name of
  "identifier" -> Identifier
  "int_literal" -> NumberLiteral
  "float_literal" -> FloatLiteral
  "comment" -> Comment
  "return_statement" -> Return
  "interpreted_string_literal" -> StringLiteral
  "raw_string_literal" -> StringLiteral
  "binary_expression" -> RelationalOperator
  "function_declaration" -> Function
  "func_literal" -> AnonymousFunction
  "call_expression" -> FunctionCall
  "selector_expression" -> SubscriptAccess
  "index_expression" -> IndexExpression
  "slice_expression" -> Slice
  "parameters" -> Args
  "short_var_declaration" -> VarDecl
  "var_spec" -> VarAssignment
  "const_spec" -> VarAssignment
  "assignment_statement" -> Assignment
  "source_file" -> Program
  "package_clause" -> Module
  "if_statement" -> If
  "for_statement" -> For
  "expression_switch_statement" -> Switch
  "type_switch_statement" -> Switch
  "expression_case_clause" -> Case
  "type_case_clause" -> Case
  "select_statement" -> Select
  "communication_case" -> Case
  "defer_statement" -> Defer
  "go_statement" -> Go
  "type_assertion_expression" -> TypeAssertion
  "type_conversion_expression" -> TypeConversion
  "keyed_element" -> Pair
  "struct_type" -> StructTy
  "map_type" -> DictionaryTy
  "array_type" -> ArrayTy
  "implicit_length_array_type" -> ArrayTy
  "parameter_declaration" -> ParameterDecl
  "expression_case" -> Case
  "type_spec" -> TypeDecl
  "field_declaration" -> FieldDecl
  "pointer_type" -> PointerTy
  "slice_type" -> SliceTy
  "element" -> Element
  "literal_value" -> Literal
  "channel_type" -> ChannelTy
  "send_statement" -> Send
  "unary_expression" -> Operator
  "function_type" -> FunctionTy
  "inc_statement" -> IncrementStatement
  "dec_statement" -> DecrementStatement
  "qualified_identifier" -> QualifiedIdentifier
  "break_statement" -> Break
  "continue_statement" -> Continue
  "rune_literal" -> RuneLiteral
  "method_declaration" -> Method
  "import_spec" -> Import
  "block" -> ExpressionStatements
  s -> Other (toS s)
