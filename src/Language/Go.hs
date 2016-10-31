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
  ("import_declaration", imports) -> do
    imports' <- sequenceA $ toImport <$> imports
    withDefaultInfo $ S.Indexed (mconcat imports')
    where
      toImport i = case toList (unwrap i) of
        [importName] -> sequenceA [ withCategory Import (S.Import importName []) ]
        xs@(_:_) -> sequenceA [ withCategory Error (S.Error xs)]
        [] -> pure []
  ("function_declaration", [id, params, block]) ->
    withDefaultInfo $ S.Function id (toList $ unwrap params) block
  (_, []) -> withDefaultInfo . S.Leaf $ toText (slice range source)
  _  -> withDefaultInfo $ S.Indexed children
  where
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
  "call_expression" -> FunctionCall
  "selector_expression" -> MethodCall
  "parameters" -> Args
  "short_var_declaration" -> VarDecl
  "assignment_statement" -> Assignment
  "source_file" -> Module
  s -> Other (toS s)

