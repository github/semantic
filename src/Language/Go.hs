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
termConstructor source sourceSpan name range children = case (name, children) of
  ("return_statement", _) -> withDefaultInfo $ S.Return (listToMaybe children)
  ("source_file", packageName : xs) | category (extract packageName) == Other "package_clause" ->
    case unwrap packageName of
      S.Indexed [identifier] -> withCategory Module (S.Module identifier xs)
      _ -> withCategory Error (S.Error $ packageName : xs)
  ("import_declaration", imports) -> toImports imports
  ("function_declaration", [id, params, block]) ->
    withDefaultInfo $ S.Function id (toList $ unwrap params) (toList $ unwrap block)
  -- TODO: Handle multiple var specs
  ("var_declaration", varSpecs) -> withDefaultInfo . S.Indexed =<< mapM toVarDecl varSpecs
  ("short_var_declaration", children) -> listToVarDecls children
  ("if_statement", children) -> toIfStatement children
  ("call_expression", [id]) -> withDefaultInfo $ S.FunctionCall id []
  ("const_declaration", constSpecs) -> toConsts constSpecs
  ("func_literal", [params, _, body]) -> withDefaultInfo $ S.AnonymousFunction (toList $ unwrap params) (toList $ unwrap body)
  (_, []) -> withDefaultInfo . S.Leaf $ toText (slice range source)
  _  -> withDefaultInfo $ S.Indexed children
  where
    toIfStatement clauses = case clauses of
      [clause, block] ->
        withDefaultInfo $ S.If clause (toList $ unwrap block)
      [expr, block, elseBlock] | category (extract block) == Other "block" ->
        withDefaultInfo $ S.If expr (toList (unwrap block) <> toList (unwrap elseBlock))
      [expr, clause, block] -> do
        clause' <- withRanges range If [expr, clause] (S.Indexed [expr, clause])
        withDefaultInfo $ S.If clause' (toList $ unwrap block)
      _ -> withCategory Error (S.Error clauses)
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
        _ -> withCategory Error (S.Error [constSpec])

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
  "selector_expression" -> MethodCall
  "parameters" -> Args
  "short_var_declaration" -> VarDecl
  "var_declaration" -> VarDecl
  "var_spec" -> VarAssignment
  "assignment_statement" -> Assignment
  "source_file" -> Module
  "const_declaration" -> VarDecl
  "if_statement" -> If
  s -> Other (toS s)

