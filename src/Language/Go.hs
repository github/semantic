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
  -> IO (SyntaxTerm Text '[Range, Category, SourceSpan]) -- ^ The resulting term, in IO.
termConstructor source sourceSpan name range children = case name of
  "return_statement" -> withDefaultInfo $ S.Return (listToMaybe children)
  "source_file" -> case children of
    packageName : rest | category (extract packageName) == Other "package_clause" ->
      case unwrap packageName of
        S.Indexed [id] -> withCategory Module (S.Module id rest)
        _ -> withCategory Error (S.Error children)
    _ -> withCategory Error (S.Error children)
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
  "expression_switch_statement" ->
    case Prologue.break isCaseClause children of
      (clauses, cases) -> do
        clauses' <- withDefaultInfo $ S.Indexed clauses
        withDefaultInfo $ S.Switch clauses' cases
      where isCaseClause = (== Case) . category . extract
  "type_switch_statement" ->
    case Prologue.break isCaseClause children of
      (clauses, cases) -> do
        withDefaultInfo $ case clauses of
          [id] -> S.Switch id cases
          _ -> S.Error children
      where isCaseClause = (== Case) . category . extract
  "select_statement" -> withDefaultInfo $ S.Select (toCommunicationCase =<< children)
    where toCommunicationCase = toList . unwrap
  "go_statement" -> withDefaultInfo $ toExpression S.Go children
  "defer_statement" -> withDefaultInfo $ toExpression S.Defer children
  "selector_expression" -> withDefaultInfo $ toSubscriptAccess children
  "index_expression" -> withDefaultInfo $ toSubscriptAccess children
  "slice_expression" -> sliceToSubscriptAccess children
  "type_assertion_expression" -> withDefaultInfo $ case children of
    [a, b] -> S.TypeAssertion a b
    rest -> S.Error rest
  "type_conversion_expression" -> withDefaultInfo $ case children of
    [a, b] -> S.TypeConversion a b
    rest -> S.Error rest
  -- TODO: Handle multiple var specs
  "var_declaration" -> withDefaultInfo . S.Indexed =<< mapM toVarDecl children
  "short_var_declaration" -> listToVarDecls children
  "if_statement" -> toIfStatement children
  "call_expression" -> withDefaultInfo $ case children of
    [id] -> S.FunctionCall id []
    rest -> S.Error rest
  "const_declaration" -> toConsts children
  "func_literal" -> withDefaultInfo $ case children of
    [params, _, body] -> S.AnonymousFunction (toList $ unwrap params) (toList $ unwrap body)
    rest -> S.Error rest
  _ -> withDefaultInfo $ case children of
    [] -> S.Leaf . toText $ slice range source
    _ -> S.Indexed children
  where
    toExpression f = \case
      [expr] -> f expr
      rest -> S.Error rest
    toSubscriptAccess = \case
      [a, b] -> S.SubscriptAccess a b
      rest -> S.Error rest
    sliceToSubscriptAccess = \case
      a : rest -> do
        slice <- withRanges range Slice rest $ S.Fixed rest
        withDefaultInfo $ S.SubscriptAccess a slice
      rest -> withDefaultInfo $ S.Error rest

    toIfStatement = \case
      [clause, block] ->
        withDefaultInfo $ S.If clause (toList $ unwrap block)
      [expr, block, elseBlock] | category (extract block) == Other "block" ->
        withDefaultInfo $ S.If expr (toList (unwrap block) <> toList (unwrap elseBlock))
      [expr, clause, block] -> do
        clause' <- withRanges range If [expr, clause] (S.Indexed [expr, clause])
        withDefaultInfo $ S.If clause' (toList $ unwrap block)
      rest -> withCategory Error (S.Error rest)
    toImports imports = do
      imports' <- mapM toImport imports
      withDefaultInfo $ S.Indexed (mconcat imports')
      where
        toImport i = case toList (unwrap i) of
          [importName] -> sequenceA [ withCategory Import (S.Import importName []) ]
          xs@(_:_) -> sequenceA [ withCategory Error (S.Error xs)]
          [] -> pure []

    toVarDecl varSpec = listToVarDecls (toList $ unwrap varSpec)

    listToVarDecls list = case list of
      [idList, exprs] | category (extract exprs) == Other "expression_list" -> do
        assignments' <- sequenceA $ zipWith (\id expr -> withDefaultInfo $ S.VarAssignment id expr) (toList $ unwrap idList) (toList $ unwrap exprs)
        withDefaultInfo (S.Indexed assignments')
      [idList, _, exprs] -> do
        assignments' <- sequenceA $ zipWith (\id expr -> withDefaultInfo $ S.VarAssignment id expr) (toList $ unwrap idList) (toList $ unwrap exprs)
        withDefaultInfo (S.Indexed assignments')
      idList : _ -> do
         varDecls <- mapM (withDefaultInfo . S.VarDecl) (toList $ unwrap idList)
         withDefaultInfo (S.Indexed varDecls)
      _ -> withCategory Error (S.Error list)

    toConsts constSpecs = do
      assignments' <- sequenceA $ toVarAssignment <$> constSpecs
      withDefaultInfo (S.Indexed assignments')
    toVarAssignment constSpec =
      case toList (unwrap constSpec) of
        [idList, expressionList] -> do
          assignments' <- sequenceA $ zipWith (\id expr -> withDefaultInfo $ S.VarAssignment id expr) (toList $ unwrap idList) (toList $ unwrap expressionList)
          withDefaultInfo (S.Indexed assignments')
        [idList, _, expressionList] -> do
          assignments' <- sequenceA $ zipWith (\id expr -> withDefaultInfo $ S.VarAssignment id expr) (toList $ unwrap idList) (toList $ unwrap expressionList)
          withDefaultInfo (S.Indexed assignments')
        [idList] -> do
           varDecls <- mapM (withDefaultInfo . S.VarDecl) (toList $ unwrap idList)
           withDefaultInfo (S.Indexed varDecls)
        rest -> withCategory Error (S.Error rest)

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
  "comment" -> Comment
  "return_statement" -> Return
  "interpreted_string_literal" -> StringLiteral
  "raw_string_literal" -> StringLiteral
  "binary_expression" -> RelationalOperator
  "function_declaration" -> Function
  "func_literal" -> AnonymousFunction
  "call_expression" -> FunctionCall
  "selector_expression" -> SubscriptAccess
  "index_expression" -> SubscriptAccess
  "slice_expression" -> SubscriptAccess
  "parameters" -> Args
  "short_var_declaration" -> VarDecl
  "var_declaration" -> VarDecl
  "var_spec" -> VarAssignment
  "assignment_statement" -> Assignment
  "source_file" -> Module
  "const_declaration" -> VarDecl
  "if_statement" -> If
  "for_statement" -> For
  "expression_switch_statement" -> Switch
  "expression_case_clause" -> Case
  "type_switch_statement" -> Switch
  "type_case_clause" -> Case
  "select_statement" -> Select
  "communication_case" -> Case
  "defer_statement" -> Defer
  "go_statement" -> Go
  "type_assertion_expression" -> TypeAssertion
  "type_conversion_expression" -> TypeConversion
  s -> Other (toS s)

