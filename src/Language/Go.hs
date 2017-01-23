{-# LANGUAGE DataKinds, GADTs #-}
module Language.Go where

import Prologue
import Info
import Source
import Term
import qualified Syntax as S
import Data.Record
import Range (unionRangesFrom)
import SourceSpan (unionSourceSpansFrom)

termAssignment
  :: Source Char -- ^ The source of the term.
  -> Record '[Range, Category, SourceSpan] -- ^ The proposed annotation for the term.
  -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ The child nodes of the term.
  -> Maybe (SyntaxTerm Text '[Range, Category, SourceSpan]) -- ^ The resulting term, in IO.
termAssignment source (range :. category :. sourceSpan :. Nil) children = case (category, children) of
  (Return, _) -> withDefaultInfo $ S.Return children
  (Import, [importName]) -> withDefaultInfo $ S.Import importName []
  (Function, [id, params, block]) -> withDefaultInfo $ S.Function id (toList $ unwrap params) (toList $ unwrap block)
  (For, [body]) | Other "block" <- Info.category (extract body) -> withDefaultInfo $ S.For [] (toList (unwrap body))
  (For, [forClause, body]) | Other "for_clause" <- Info.category (extract forClause) -> withDefaultInfo $ S.For (toList (unwrap forClause)) (toList (unwrap body))
  (For, [rangeClause, body]) | Other "range_clause" <- Info.category (extract rangeClause) -> withDefaultInfo $ S.For (toList (unwrap rangeClause)) (toList (unwrap body))
  (TypeDecl, [identifier, ty]) -> withDefaultInfo $ S.TypeDecl identifier ty
  (StructTy, _) -> withDefaultInfo (S.Ty children)
  (FieldDecl, [idList]) | [ident] <- toList (unwrap idList)
                        -> withDefaultInfo (S.FieldDecl ident Nothing Nothing)
  (FieldDecl, [idList, ty]) | [ident] <- toList (unwrap idList)
                            -> withDefaultInfo $ case Info.category (extract ty) of
                                StringLiteral -> S.FieldDecl ident Nothing (Just ty)
                                _ -> S.FieldDecl ident (Just ty) Nothing
  (FieldDecl, [idList, ty, tag]) | [ident] <- toList (unwrap idList)
                                 -> withDefaultInfo (S.FieldDecl ident (Just ty) (Just tag))
  (Switch, _) ->
    withDefaultInfo $ case Prologue.break ((== Case) . Info.category . extract) children of
      ([id], cases) -> S.Switch (Just id) cases -- type_switch_statement
      ([], cases) -> S.Switch Nothing cases
      (clauses, cases) -> S.Switch (Just (withCategory ExpressionStatements (S.Indexed clauses))) cases
  (ParameterDecl, param : ty) -> withDefaultInfo $ S.ParameterDecl (listToMaybe ty) param
  (Assignment, _) | Just assignment <- toVarAssignment children -> Just assignment
  (Select, _) -> withDefaultInfo $ S.Select (toCommunicationCase =<< children)
    where toCommunicationCase = toList . unwrap
  (Go, [expr]) -> withDefaultInfo $ S.Go expr
  (Defer, [expr]) -> withDefaultInfo $ S.Defer expr
  (SubscriptAccess, [a, b]) -> withDefaultInfo $ S.SubscriptAccess a b
  (IndexExpression, [a, b]) -> withDefaultInfo $ S.SubscriptAccess a b
  (Slice, a : rest) -> withDefaultInfo (S.SubscriptAccess a (withRanges range Element (S.Fixed rest)))
  (Other "composite_literal", [ty, values]) | ArrayTy <- Info.category (extract ty)
                                            -> withDefaultInfo $ S.Array (Just ty) (toList (unwrap values))
                                            | DictionaryTy <- Info.category (extract ty)
                                            -> withDefaultInfo $ S.Object (Just ty) (toList (unwrap values))
                                            | SliceTy <- Info.category (extract ty)
                                            -> withDefaultInfo $ S.SubscriptAccess ty values
  (Other "composite_literal", []) -> withDefaultInfo $ S.Struct Nothing []
  (Other "composite_literal", [ty]) -> withDefaultInfo $ S.Struct (Just ty) []
  (Other "composite_literal", [ty, values]) -> withDefaultInfo $ S.Struct (Just ty) (toList (unwrap values))
  (TypeAssertion, [a, b]) -> withDefaultInfo $ S.TypeAssertion a b
  (TypeConversion, [a, b]) -> withDefaultInfo $ S.TypeConversion a b
  -- TODO: Handle multiple var specs
  (VarAssignment, _) | Just assignment <- toVarAssignment children -> Just assignment
  (VarDecl, _) | Just assignment <- toVarAssignment children -> Just assignment
  (If, _) -> toIfStatement children
  (FunctionCall, [id]) -> withDefaultInfo $ S.FunctionCall id []
  (FunctionCall, id : rest) -> withDefaultInfo $ S.FunctionCall id rest
  (AnonymousFunction, [params, _, body]) | [params'] <- toList (unwrap params)
                                         -> withDefaultInfo $ S.AnonymousFunction (toList (unwrap params')) (toList (unwrap body))
  (PointerTy, _) -> withDefaultInfo $ S.Ty children
  (ChannelTy, _) -> withDefaultInfo $ S.Ty children
  (Send, [channel, expr]) -> withDefaultInfo $ S.Send channel expr
  (Operator, _) -> withDefaultInfo $ S.Operator children
  (FunctionTy, _) -> withDefaultInfo $ S.Ty children
  (IncrementStatement, _) ->
    withDefaultInfo $ S.Leaf $ toText source
  (DecrementStatement, _) ->
    withDefaultInfo $ S.Leaf $ toText source
  (QualifiedIdentifier, _) ->
    withDefaultInfo $ S.Leaf $ toText source
  (Method, [params, name, fun]) -> withDefaultInfo (S.Method name Nothing (toList (unwrap params)) (toList (unwrap fun)))
  (Method, [params, name, outParams, fun]) -> withDefaultInfo (S.Method name Nothing (toList (unwrap params) <> toList (unwrap outParams)) (toList (unwrap fun)))
  (Method, [params, name, outParams, ty, fun]) -> withDefaultInfo (S.Method name (Just ty) (toList (unwrap params) <> toList (unwrap outParams)) (toList (unwrap fun)))
  _ -> Nothing
  where
    toIfStatement children = case Prologue.break ((Other "block" ==) . Info.category . extract) children of
      (clauses, blocks) ->
        let clauses' = withRanges range ExpressionStatements (S.Indexed clauses)
            blocks' = foldMap (toList . unwrap) blocks
        in withDefaultInfo (S.If clauses' blocks')

    toVarAssignment = \case
        [idList, ty] | Info.category (extract ty) == Identifier ->
          let ids = toList (unwrap idList)
              idList' = (\id -> withRanges range VarDecl (S.VarDecl id (Just ty))) <$> ids
          in Just $ withRanges range ExpressionStatements (S.Indexed idList')
        [idList, expressionList] | Info.category (extract expressionList) == Other "expression_list" ->
          let assignments' = zipWith ((withCategory VarAssignment .) . S.VarAssignment)
                (toList $ unwrap idList) (toList $ unwrap expressionList)
          in Just $ withRanges range ExpressionStatements (S.Indexed assignments')
        [idList, _, expressionList] ->
          let assignments' = zipWith ((withCategory VarAssignment .) . S.VarAssignment) (toList $ unwrap idList) (toList $ unwrap expressionList)
          in Just $ withRanges range ExpressionStatements (S.Indexed assignments')
        [idList] -> withDefaultInfo (S.Indexed [idList])
        _ -> Nothing

    withRanges originalRange category' syntax =
      cofree ((unionRangesFrom originalRange (characterRange . extract <$> toList syntax) :. category' :. unionSourceSpansFrom sourceSpan (Info.sourceSpan . extract <$> toList syntax) :. Nil) :< syntax)

    withCategory category syntax =
      cofree ((range :. category :. sourceSpan :. Nil) :< syntax)

    withDefaultInfo = Just . withCategory category

categoryForGoName :: Text -> Category
categoryForGoName = \case
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
  s -> Other (toS s)
