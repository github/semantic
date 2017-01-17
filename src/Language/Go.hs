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
  -> IO SourceSpan -- ^ The span that the term occupies. This is passed in 'IO' to guarantee some access constraints & encourage its use only when needed (improving performance).
  -> Text -- ^ The name of the production for this node.
  -> Range -- ^ The character range that the term occupies.
  -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ The child nodes of the term.
  -> IO [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ All child nodes (included unnamed productions) of the term as 'IO'. Only use this if you need it.
  -> IO (SyntaxTerm Text '[Range, Category, SourceSpan]) -- ^ The resulting term, in IO.
termConstructor source sourceSpan name range children _ = case name of
  "return_statement" -> withDefaultInfo $ S.Return children
  "source_file" -> case Prologue.break (\node -> category (extract node) == Other "package_clause") children of
    (comments, packageName : rest) -> case unwrap packageName of
        S.Indexed [id] -> do
          module' <- withCategory Module (S.Module id rest)
          withCategory Program (S.Indexed (comments <> [module']))
        _ -> withRanges range Error children (S.Error children)
    _ -> withRanges range Error children (S.Error children)
  "import_declaration" -> toImports children
  "function_declaration" -> withDefaultInfo $ case children of
    [id, params, block] -> S.Function id (toList $ unwrap params) (toList $ unwrap block)
    rest -> S.Error rest
  "for_statement" ->
    withDefaultInfo $ case children of
      [body] | category (extract body) == Other "block" ->
        S.For [] (toList $ unwrap body)
      [forClause, body] | category (extract forClause) == Other "for_clause" ->
        S.For (toList $ unwrap forClause) (toList $ unwrap body)
      [rangeClause, body] | category (extract rangeClause) == Other "range_clause" ->
        S.For (toList $ unwrap rangeClause) (toList $ unwrap body)
      other -> S.Error other
  "type_declaration" -> toTypeDecls children
  "type_spec" -> toTypeDecl children
  "struct_type" -> toStructTy children
  "field_declaration" -> toFieldDecl children
  "expression_switch_statement" ->
    case Prologue.break isCaseClause children of
      (clauses, cases) -> do
        clauses' <- case clauses of
          [] -> pure Nothing
          clauses'' -> Just <$> (withCategory ExpressionStatements (S.Indexed clauses''))
        cases' <- sequenceA $ toCase <$> cases
        withDefaultInfo $ S.Switch clauses' cases'
      where
        isCaseClause = (== Other "expression_case_clause") . category . extract
        toCase clause = case toList (unwrap clause) of
          clause' : rest -> case toList (unwrap clause') of
            [clause''] -> withCategory Case $ S.Case clause'' rest
            [] -> withCategory Default $ S.Default rest
            rest -> withCategory Error $ S.Error rest
          [] -> withCategory Error $ S.Error [clause]
  "parameter_declaration" -> withDefaultInfo $ case children of
    [param, ty] -> S.ParameterDecl (Just ty) param
    [param] -> S.ParameterDecl Nothing param
    _ -> S.Error children
  "assignment_statement" -> toVarAssignment children
  "type_switch_statement" ->
    case Prologue.break isCaseClause children of
      (clauses, cases) -> do
        withDefaultInfo $ case clauses of
          [id] -> S.Switch (Just id) cases
          _ -> S.Error children
      where isCaseClause = (== Case) . category . extract
  "select_statement" -> withDefaultInfo $ S.Select (toCommunicationCase =<< children)
    where toCommunicationCase = toList . unwrap
  "go_statement" -> withDefaultInfo $ toExpression S.Go children
  "defer_statement" -> withDefaultInfo $ toExpression S.Defer children
  "selector_expression" -> withDefaultInfo $ toSubscriptAccess children
  "index_expression" -> withDefaultInfo $ toSubscriptAccess children
  "slice_expression" -> sliceToSubscriptAccess children
  "composite_literal" -> toLiteral children
  "type_assertion_expression" -> withDefaultInfo $ case children of
    [a, b] -> S.TypeAssertion a b
    rest -> S.Error rest
  "type_conversion_expression" -> withDefaultInfo $ case children of
    [a, b] -> S.TypeConversion a b
    rest -> S.Error rest
  -- TODO: Handle multiple var specs
  "var_declaration" -> toVarDecls children
  "var_spec" -> toVarAssignment children
  "short_var_declaration" -> toVarAssignment children
  "if_statement" -> toIfStatement children
  "call_expression" -> withDefaultInfo $ case children of
    [id] -> S.FunctionCall id []
    id : rest -> S.FunctionCall id rest
    rest -> S.Error rest
  "const_declaration" -> toConsts children
  "const_spec" -> toVarAssignment children
  "func_literal" -> withDefaultInfo $ case children of
    [params, _, body] -> case toList (unwrap params) of
      [params'] -> S.AnonymousFunction (toList $ unwrap params') (toList $ unwrap body)
      rest -> S.Error rest
    rest -> S.Error rest
  "pointer_type" -> withDefaultInfo $ case children of
    [ty] -> S.Ty ty
    rest -> S.Error rest
  "channel_type" -> withDefaultInfo $ case children of
    [ty] -> S.Ty ty
    rest -> S.Error rest
  "send_statement" -> withDefaultInfo $ case children of
    [channel, expr] -> S.Send channel expr
    rest -> S.Error rest
  "unary_expression" -> withDefaultInfo $ S.Operator children
  "function_type" -> do
    params <- withRanges range Params children $ S.Indexed children
    withDefaultInfo $ S.Ty params
  "inc_statement" -> do
    withDefaultInfo $ S.Leaf . toText $ slice range source
  "dec_statement" -> do
    withDefaultInfo $ S.Leaf . toText $ slice range source
  "qualified_identifier" -> do
    withDefaultInfo $ S.Leaf . toText $ slice range source
  _ -> withDefaultInfo $ case children of
    [] -> S.Leaf . toText $ slice range source
    _ -> S.Indexed children
  where
    toStructTy children = do
      fields <- withRanges range FieldDeclarations children (S.Indexed children)
      withDefaultInfo (S.Ty fields)

    toLiteral = \case
      children@[ty, _] -> case category (extract ty) of
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
      [identifier, ty] -> case category (extract ty) of
        StringLiteral -> withCategory FieldDecl (S.FieldDecl identifier Nothing (Just ty))
        _ -> withCategory FieldDecl (S.FieldDecl identifier (Just ty) Nothing)
      [identifier] -> withCategory FieldDecl (S.FieldDecl identifier Nothing Nothing)
      [identifier, ty, tag] -> withCategory FieldDecl (S.FieldDecl identifier (Just ty) (Just tag))
      rest -> withRanges range Error rest (S.Error rest)

    toExpression f = \case
      [expr] -> f expr
      rest -> S.Error rest
    toSubscriptAccess = \case
      [a, b] -> S.SubscriptAccess a b
      rest -> S.Error rest
    sliceToSubscriptAccess = \case
      a : rest -> do
        sliceElement <- withRanges range Element rest $ S.Fixed rest
        withCategory Slice (S.SubscriptAccess a sliceElement)
      rest -> withRanges range Error rest $ S.Error rest

    toIfStatement children = case Prologue.break ((Other "block" ==) . category . extract) children of
      (clauses, blocks) -> do
        clauses' <- withRanges range ExpressionStatements clauses (S.Indexed clauses)
        let blocks' = foldMap (toList . unwrap) blocks
        withDefaultInfo (S.If clauses' blocks')

    toTypeDecls types = withDefaultInfo $ S.Indexed types

    toTypeDecl = \case
      [identifier, ty] -> withDefaultInfo $ S.TypeDecl identifier ty
      rest -> withRanges range Error rest $ S.Error rest

    toImports imports = do
      imports' <- mapM toImport imports
      withDefaultInfo $ S.Indexed (mconcat imports')
      where
        toImport i = case toList (unwrap i) of
          [importName] -> sequenceA [ withCategory Import (S.Import importName []) ]
          rest@(_:_) -> sequenceA [ withRanges range Error rest (S.Error rest)]
          [] -> pure []

    toVarDecls children = withDefaultInfo (S.Indexed children)

    toConsts constSpecs = do
      assignments' <- mapM constSpecToVarAssignment constSpecs
      withDefaultInfo (S.Indexed assignments')
    constSpecToVarAssignment = toVarAssignment . toList . unwrap
    toVarAssignment = \case
        [idList, ty] | category (extract ty) == Identifier -> do
          let ids = toList (unwrap idList)
          idList' <- mapM (\id -> withRanges range VarDecl [id] (S.VarDecl id (Just ty))) ids
          withRanges range ExpressionStatements idList' (S.Indexed idList')
        [idList, expressionList] | category (extract expressionList) == Other "expression_list" -> do
          assignments' <- sequenceA $ zipWith (\id expr ->
            withCategory VarAssignment $ S.VarAssignment id expr)
            (toList $ unwrap idList) (toList $ unwrap expressionList)
          withDefaultInfo (S.Indexed assignments')
        [idList, _, expressionList] -> do
          assignments' <- sequenceA $ zipWith (\id expr ->
            withCategory VarAssignment $ S.VarAssignment id expr) (toList $ unwrap idList) (toList $ unwrap expressionList)
          withDefaultInfo (S.Indexed assignments')
        [idList] -> withDefaultInfo (S.Indexed [idList])
        rest -> withRanges range Error rest (S.Error rest)

    withRanges originalRange category' terms syntax = do
      let ranges' = getField . extract <$> terms
      sourceSpan' <- sourceSpan
      let sourceSpans' = getField . extract <$> terms
      pure $! cofree ((unionRangesFrom originalRange ranges' .: category' .: unionSourceSpansFrom sourceSpan' sourceSpans' .: RNil) :< syntax)

    withCategory category syntax = do
      sourceSpan' <- sourceSpan
      pure $! cofree ((range .: category .: sourceSpan' .: RNil) :< syntax)

    withDefaultInfo = withCategory (categoryForGoName name)

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
  "assignment_statement" -> Assignment
  "source_file" -> Module
  "if_statement" -> If
  "for_statement" -> For
  "expression_switch_statement" -> Switch
  "type_switch_statement" -> Switch
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
  "type_declaration" -> TypeDecl
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
  s -> Other (toS s)
