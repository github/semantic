{-# LANGUAGE DataKinds #-}
module Language.Go where

import Prologue
import Info
import Source
import Term
import qualified Syntax as S
import Data.Record

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
    withDefaultInfo $ S.Function id (toList $ unwrap params) block
  -- TODO: Handle multiple var specs
  ("var_declaration", [varSpec]) -> toVarDecl varSpec
  ("call_expression", [id]) -> withDefaultInfo $ S.FunctionCall id []
  ("const_declaration", constSpecs) -> toConsts constSpecs
  ("func_literal", [params, _, body]) -> withDefaultInfo $ S.AnonymousFunction (toList $ unwrap params) body
  (_, []) -> withDefaultInfo . S.Leaf $ toText (slice range source)
  _  -> withDefaultInfo $ S.Indexed children
  where
    toImports imports = do
      imports' <- sequenceA $ toImport <$> imports
      withDefaultInfo $ S.Indexed (mconcat imports')
      where
        toImport i = case toList (unwrap i) of
          [importName] -> sequenceA [ withCategory Import (S.Import importName []) ]
          xs@(_:_) -> sequenceA [ withCategory Error (S.Error xs)]
          [] -> pure []

    toVarDecl varSpec = do
      assignment' <- case toList (unwrap varSpec) of
        [idList, _, exprs] -> do
          identifier' <- idOrError idList
          withDefaultInfo $ S.VarAssignment identifier' exprs
        _ -> withCategory Error (S.Error [varSpec])
      pure assignment'

    idOrError idList = case toList (unwrap idList) of
      [identifier] -> pure identifier
      _ -> withCategory Error (S.Error [idList])

    toConsts constSpecs = do
      assignments' <- sequenceA $ toVarAssignment <$> constSpecs
      withDefaultInfo (S.Indexed assignments')
    toVarAssignment constSpec = do
      assignment <- case toList (unwrap constSpec) of
        idList : rest -> do
          identifier' <- case toList (unwrap idList) of
            id : _ -> case toList (unwrap id) of
              id : _ -> pure id
              _ -> withCategory Error (S.Error [constSpec])
            _ -> withCategory Error (S.Error [constSpec])
          rest' <- withDefaultInfo (S.Indexed rest)
          withDefaultInfo $ S.VarAssignment identifier' rest'
        _ -> withCategory Error (S.Error [constSpec])
      withDefaultInfo $ S.VarDecl assignment
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
  s -> Other (toS s)

