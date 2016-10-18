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
  -> [Term (S.Syntax Text) (Record '[Range, Category, SourceSpan])] -- ^ The child nodes of the term.
  -> IO (Term (S.Syntax Text) (Record '[Range, Category, SourceSpan])) -- ^ The resulting term, in IO.
termConstructor source sourceSpan name range children = withDefaultInfo =<< do
  putStrLn name
  pure $ case (name, children) of
    ("return_statement", _) -> S.Return (listToMaybe children)
    (_, []) -> S.Leaf . toText $ slice range source
    _  -> S.Indexed children
  where
    withDefaultInfo syntax = do
      sourceSpan' <- sourceSpan
      pure $! cofree ((range .: categoryForGoName name .: sourceSpan' .: RNil) :< syntax)

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
  "source_file" -> Program
  s -> Other (toS s)

