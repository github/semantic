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
termAssignment source (range :. category :. sourceSpan :. Nil) children = Just $ case (category, children) of
  (Return, _) -> withDefaultInfo $ S.Return children
  (Module, _) | (comments, packageName : rest) <- Prologue.break ((== Other "package_clause") . Info.category . extract) children
              , S.Indexed [id] <- unwrap packageName
              -> withCategory Program (S.Indexed (comments <> [withCategory Module (S.Module id rest)]))
  (Other "import_declaration", _) -> toImports children
  (Function, [id, params, block]) -> withDefaultInfo $ S.Function id (toList $ unwrap params) (toList $ unwrap block)
  (For, [body]) | Other "block" <- Info.category (extract body) -> withDefaultInfo $ S.For [] (toList (unwrap body))
  (For, [forClause, body]) | Other "for_clause" <- Info.category (extract forClause) -> withDefaultInfo $ S.For (toList (unwrap forClause)) (toList (unwrap body))
  (For, [rangeClause, body]) | Other "range_clause" <- Info.category (extract rangeClause) -> withDefaultInfo $ S.For (toList (unwrap rangeClause)) (toList (unwrap body))
  (TypeDecl, [identifier, ty]) -> withDefaultInfo $ S.TypeDecl identifier ty
  (StructTy, _) -> toStructTy children
  (FieldDecl, _) -> toFieldDecl children
  (Switch, _) ->
    withDefaultInfo $ case Prologue.break ((== Case) . Info.category . extract) children of
      ([id], cases) -> S.Switch (Just id) cases -- type_switch_statement
      ([], cases) -> S.Switch Nothing cases
      (clauses, cases) -> S.Switch (Just (withCategory ExpressionStatements (S.Indexed clauses))) cases
  (ParameterDecl, param : ty) -> withDefaultInfo $ S.ParameterDecl (listToMaybe ty) param
  (Assignment, _) -> toVarAssignment children
  (Select, _) -> withDefaultInfo $ S.Select (toCommunicationCase =<< children)
    where toCommunicationCase = toList . unwrap
  (Go, [expr]) -> withDefaultInfo $ S.Go expr
  (Defer, [expr]) -> withDefaultInfo $ S.Defer expr
  (SubscriptAccess, [a, b]) -> withDefaultInfo $ S.SubscriptAccess a b
  (IndexExpression, [a, b]) -> withDefaultInfo $ S.SubscriptAccess a b
  (Slice, a : rest) -> withCategory Slice (S.SubscriptAccess a (withRanges range Element rest (S.Fixed rest)))
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
  (Other "var_declaration", _) -> toVarDecls children
  (VarAssignment, _) -> toVarAssignment children
  (VarDecl, _) -> toVarAssignment children
  (If, _) -> toIfStatement children
  (FunctionCall, [id]) -> withDefaultInfo $ S.FunctionCall id []
  (FunctionCall, id : rest) -> withDefaultInfo $ S.FunctionCall id rest
  (Other "const_declaration", _) -> toConsts children
  (AnonymousFunction, [params, _, body]) | [params'] <- toList (unwrap params)
                                         -> withDefaultInfo $ S.AnonymousFunction (toList (unwrap params')) (toList (unwrap body))
  (PointerTy, [ty]) -> withDefaultInfo $ S.Ty ty
  (ChannelTy, [ty]) -> withDefaultInfo $ S.Ty ty
  (Send, [channel, expr]) -> withDefaultInfo $ S.Send channel expr
  (Operator, _) -> withDefaultInfo $ S.Operator children
  (FunctionTy, _) ->
    let params = withRanges range Params children $ S.Indexed children
    in withDefaultInfo $ S.Ty params
  (IncrementStatement, _) ->
    withDefaultInfo $ S.Leaf $ toText source
  (DecrementStatement, _) ->
    withDefaultInfo $ S.Leaf $ toText source
  (QualifiedIdentifier, _) ->
    withDefaultInfo $ S.Leaf $ toText source
  (Break, [label]) -> withDefaultInfo $ S.Break (Just label)
  (Break, []) -> withDefaultInfo $ S.Break Nothing
  (Continue, [label]) -> withDefaultInfo $ S.Continue (Just label)
  (Continue, []) -> withDefaultInfo $ S.Continue Nothing
  (Pair, [key, value]) -> withDefaultInfo $ S.Pair key value
  (Method, [params, name, fun]) -> withDefaultInfo (S.Method name Nothing (toList (unwrap params)) (toList (unwrap fun)))
  (Method, [params, name, outParams, fun]) -> withDefaultInfo (S.Method name Nothing (toList (unwrap params) <> toList (unwrap outParams)) (toList (unwrap fun)))
  (Method, [params, name, outParams, ty, fun]) -> withDefaultInfo (S.Method name (Just ty) (toList (unwrap params) <> toList (unwrap outParams)) (toList (unwrap fun)))
  _ -> withDefaultInfo $ case children of
    [] -> S.Leaf $ toText source
    _ -> S.Indexed children
  where
    toStructTy children =
      withDefaultInfo (S.Ty (withRanges range FieldDeclarations children (S.Indexed children)))

    toFieldDecl = \case
      [idList, ty] ->
        case Info.category (extract ty) of
          StringLiteral -> withCategory FieldDecl (S.FieldDecl (toIdent (toList (unwrap idList))) Nothing (Just ty))
          _ -> withCategory FieldDecl (S.FieldDecl (toIdent (toList (unwrap idList))) (Just ty) Nothing)
      [idList] ->
        withCategory FieldDecl (S.FieldDecl (toIdent (toList (unwrap idList))) Nothing Nothing)
      [idList, ty, tag] ->
        withCategory FieldDecl (S.FieldDecl (toIdent (toList (unwrap idList))) (Just ty) (Just tag))
      rest -> withRanges range Error rest (S.Error rest)

      where
        toIdent = \case
          [ident] -> ident
          rest -> withRanges range Error rest (S.Error rest)

    toIfStatement children = case Prologue.break ((Other "block" ==) . Info.category . extract) children of
      (clauses, blocks) ->
        let clauses' = withRanges range ExpressionStatements clauses (S.Indexed clauses)
            blocks' = foldMap (toList . unwrap) blocks
        in withDefaultInfo (S.If clauses' blocks')

    toImports imports =
      withDefaultInfo $ S.Indexed (imports >>= toImport)
      where
        toImport i = case toList (unwrap i) of
          [importName] -> [ withCategory Import (S.Import importName []) ]
          rest@(_:_) -> [ withRanges range Error rest (S.Error rest)]
          [] -> []

    toVarDecls children = withDefaultInfo (S.Indexed children)

    toConsts constSpecs = withDefaultInfo (S.Indexed constSpecs)

    toVarAssignment = \case
        [idList, ty] | Info.category (extract ty) == Identifier ->
          let ids = toList (unwrap idList)
              idList' = (\id -> withRanges range VarDecl [id] (S.VarDecl id (Just ty))) <$> ids
          in withRanges range ExpressionStatements idList' (S.Indexed idList')
        [idList, expressionList] | Info.category (extract expressionList) == Other "expression_list" ->
          let assignments' = zipWith (\id expr ->
                withCategory VarAssignment $ S.VarAssignment id expr)
                (toList $ unwrap idList) (toList $ unwrap expressionList)
          in withRanges range ExpressionStatements assignments' (S.Indexed assignments')
        [idList, _, expressionList] ->
          let assignments' = zipWith (\id expr ->
                withCategory VarAssignment $ S.VarAssignment id expr) (toList $ unwrap idList) (toList $ unwrap expressionList)
          in withRanges range ExpressionStatements assignments' (S.Indexed assignments')
        [idList] -> withDefaultInfo (S.Indexed [idList])
        rest -> withRanges range Error rest (S.Error rest)

    withRanges originalRange category' terms syntax =
      let ranges' = getField . extract <$> terms
          sourceSpans' = getField . extract <$> terms
      in
      cofree ((unionRangesFrom originalRange ranges' :. category' :. unionSourceSpansFrom sourceSpan sourceSpans' :. Nil) :< syntax)

    withCategory category syntax =
      cofree ((range :. category :. sourceSpan :. Nil) :< syntax)

    withDefaultInfo = withCategory category

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
  "source_file" -> Module
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
  s -> Other (toS s)
