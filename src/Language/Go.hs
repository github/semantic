{-# LANGUAGE DataKinds #-}
module Language.Go where

import Prologue
import Info
import Source
import Term
import qualified Syntax as S
import Data.Record
import Range (unionRangesFrom)
import SourceSpan (unionSourceSpansFrom)

termConstructor
  :: Source Char -- ^ The source that the term occurs within.
  -> SourceSpan -- ^ The span that the term occupies.
  -> Category -- ^ The node’s Category.
  -> Range -- ^ The character range that the term occupies.
  -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ The child nodes of the term.
  -> IO [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ All child nodes (included unnamed productions) of the term as 'IO'. Only use this if you need it.
  -> IO (SyntaxTerm Text '[Range, Category, SourceSpan]) -- ^ The resulting term, in IO.
termConstructor source sourceSpan category range children _ = pure $! case category of
  Return -> withDefaultInfo $ S.Return children
  Module -> case Prologue.break (\node -> Info.category (extract node) == Other "package_clause") children of
    (comments, packageName : rest) -> case unwrap packageName of
        S.Indexed [id] ->
          let module' = withCategory Module (S.Module id rest)
          in withCategory Program (S.Indexed (comments <> [module']))
        _ -> withRanges range Error children (S.Error children)
    _ -> withRanges range Error children (S.Error children)
  Other "import_declaration" -> toImports children
  Function -> withDefaultInfo $ case children of
    [id, params, block] -> S.Function id (toList $ unwrap params) Nothing (toList $ unwrap block)
    [id, params, ty, block] -> S.Function id (toList $ unwrap params) (Just ty) (toList $ unwrap block)
    rest -> S.Error rest
  For ->
    withDefaultInfo $ case children of
      [body] | Info.category (extract body) == Other "block" ->
        S.For [] (toList $ unwrap body)
      [forClause, body] | Info.category (extract forClause) == Other "for_clause" ->
        S.For (toList $ unwrap forClause) (toList $ unwrap body)
      [rangeClause, body] | Info.category (extract rangeClause) == Other "range_clause" ->
        S.For (toList $ unwrap rangeClause) (toList $ unwrap body)
      other -> S.Error other
  TypeDecl -> toTypeDecl children
  StructTy -> toStructTy children
  FieldDecl -> toFieldDecl children
  Switch ->
    case Prologue.break isCaseClause children of
      (clauses, cases) -> withDefaultInfo $ case clauses of
        [id] -> S.Switch (Just id) cases -- type_switch_statement
        [] -> S.Switch Nothing (toCase <$> cases)
        _ -> S.Switch (Just (withCategory ExpressionStatements (S.Indexed clauses))) (toCase <$> cases)
      where
        isCaseClause = (== Case) . Info.category . extract
        toCase clause = case toList (unwrap clause) of
          clause' : rest -> case toList (unwrap clause') of
            [clause''] -> withCategory Case $ S.Case clause'' rest
            [] -> withCategory DefaultCase $ S.DefaultCase rest
            rest -> withCategory Error $ S.Error rest
          [] -> withCategory Error $ S.Error [clause]
  ParameterDecl -> withDefaultInfo $ case children of
    [param, ty] -> S.ParameterDecl (Just ty) param
    [param] -> S.ParameterDecl Nothing param
    _ -> S.Error children
  Assignment -> toVarAssignment children
  Select -> withDefaultInfo $ S.Select (toCommunicationCase =<< children)
    where toCommunicationCase = toList . unwrap
  Go -> withDefaultInfo $ toExpression S.Go children
  Defer -> withDefaultInfo $ toExpression S.Defer children
  SubscriptAccess -> withDefaultInfo $ toSubscriptAccess children
  IndexExpression -> withDefaultInfo $ toSubscriptAccess children
  Slice -> sliceToSubscriptAccess children
  Other "composite_literal" -> toLiteral children
  TypeAssertion -> withDefaultInfo $ case children of
    [a, b] -> S.TypeAssertion a b
    rest -> S.Error rest
  TypeConversion -> withDefaultInfo $ case children of
    [a, b] -> S.TypeConversion a b
    rest -> S.Error rest
  -- TODO: Handle multiple var specs
  Other "var_declaration" -> toVarDecls children
  VarAssignment -> toVarAssignment children
  VarDecl -> toVarAssignment children
  If -> toIfStatement children
  FunctionCall -> withDefaultInfo $ case children of
    [id] -> S.FunctionCall id []
    id : rest -> S.FunctionCall id rest
    rest -> S.Error rest
  Other "const_declaration" -> toConsts children
  AnonymousFunction -> withDefaultInfo $ case children of
    [params, _, body] -> case toList (unwrap params) of
      [params'] -> S.AnonymousFunction (toList $ unwrap params') (toList $ unwrap body)
      rest -> S.Error rest
    rest -> S.Error rest
  PointerTy -> withDefaultInfo $ case children of
    [ty] -> S.Ty ty
    rest -> S.Error rest
  ChannelTy -> withDefaultInfo $ case children of
    [ty] -> S.Ty ty
    rest -> S.Error rest
  Send -> withDefaultInfo $ case children of
    [channel, expr] -> S.Send channel expr
    rest -> S.Error rest
  Operator -> withDefaultInfo $ S.Operator children
  FunctionTy ->
    let params = withRanges range Params children $ S.Indexed children
    in withDefaultInfo $ S.Ty params
  IncrementStatement ->
    withDefaultInfo $ S.Leaf . toText $ slice range source
  DecrementStatement ->
    withDefaultInfo $ S.Leaf . toText $ slice range source
  QualifiedIdentifier ->
    withDefaultInfo $ S.Leaf . toText $ slice range source
  Break -> toBreak children
  Continue -> toContinue children
  Pair -> toPair children
  Method -> toMethod children
  _ -> withDefaultInfo $ case children of
    [] -> S.Leaf . toText $ slice range source
    _ -> S.Indexed children
  where
    toMethod = \case
      [params, name, fun] -> withDefaultInfo (S.Method name Nothing (toList $ unwrap params) (toList $ unwrap fun))
      [params, name, outParams, fun] ->
        let params' = toList (unwrap params)
            outParams' = toList (unwrap outParams)
            allParams = params' <> outParams'
        in withDefaultInfo (S.Method name Nothing allParams (toList $ unwrap fun))
      [params, name, outParams, ty, fun] ->
        let params' = toList (unwrap params)
            outParams' = toList (unwrap outParams)
            allParams = params' <> outParams'
        in withDefaultInfo (S.Method name (Just ty) allParams (toList $ unwrap fun))
      rest -> withCategory Error (S.Error rest)
    toPair = \case
      [key, value] -> withDefaultInfo (S.Pair key value)
      rest -> withCategory Error (S.Error rest)
    toBreak = \case
      [label] -> withDefaultInfo (S.Break (Just label))
      [] -> withDefaultInfo (S.Break Nothing)
      rest -> withCategory Error (S.Error rest)
    toContinue = \case
      [label] -> withDefaultInfo (S.Continue (Just label))
      [] -> withDefaultInfo (S.Continue Nothing)
      rest -> withCategory Error (S.Error rest)

    toStructTy children =
      withDefaultInfo (S.Ty (withRanges range FieldDeclarations children (S.Indexed children)))

    toLiteral = \case
      children@[ty, _] -> case Info.category (extract ty) of
        ArrayTy -> toImplicitArray children
        DictionaryTy -> toMap children
        SliceTy -> sliceToSubscriptAccess children
        _ -> toStruct children
      rest -> withRanges range Error rest $ S.Error rest
    toImplicitArray = \case
      [ty, values] -> withCategory ArrayLiteral (S.Array (Just ty) (toList $ unwrap values))
      rest -> withRanges range Error rest $ S.Error rest
    toMap = \case
      [ty, values] -> withCategory DictionaryLiteral (S.Object (Just ty) (toList $ unwrap values))
      rest -> withRanges range Error rest $ S.Error rest
    toStruct = \case
      [] -> withCategory Struct (S.Struct Nothing [])
      [ty] -> withCategory Struct (S.Struct (Just ty) [])
      [ty, values] -> withCategory Struct (S.Struct (Just ty) (toList $ unwrap values))
      rest -> withRanges range Error rest $ S.Error rest
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


    toExpression f = \case
      [expr] -> f expr
      rest -> S.Error rest
    toSubscriptAccess = \case
      [a, b] -> S.SubscriptAccess a b
      rest -> S.Error rest
    sliceToSubscriptAccess = \case
      a : rest ->
        let sliceElement = withRanges range Element rest $ S.Fixed rest
        in withCategory Slice (S.SubscriptAccess a sliceElement)
      rest -> withRanges range Error rest $ S.Error rest

    toIfStatement children = case Prologue.break ((Other "block" ==) . Info.category . extract) children of
      (clauses, blocks) ->
        let clauses' = withRanges range ExpressionStatements clauses (S.Indexed clauses)
            blocks' = foldMap (toList . unwrap) blocks
        in withDefaultInfo (S.If clauses' blocks')

    toTypeDecl = \case
      [identifier, ty] -> withDefaultInfo $ S.TypeDecl identifier ty
      rest -> withRanges range Error rest $ S.Error rest

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
  "ERROR" -> Error
  "function_type" -> FunctionTy
  "inc_statement" -> IncrementStatement
  "dec_statement" -> DecrementStatement
  "qualified_identifier" -> QualifiedIdentifier
  "break_statement" -> Break
  "continue_statement" -> Continue
  "rune_literal" -> RuneLiteral
  "method_declaration" -> Method
  s -> Other (toS s)
