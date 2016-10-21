{-# LANGUAGE DataKinds #-}
module Language.Ruby where

import Data.Record
import Info
import Prologue
import Source
import qualified Syntax as S
import Term

termConstructor
  :: Source Char -- ^ The source that the term occurs within.
  -> IO SourceSpan -- ^ The span that the term occupies. This is passed in 'IO' to guarantee some access constraints & encourage its use only when needed (improving performance).
  -> Text -- ^ The name of the production for this node.
  -> Range -- ^ The character range that the term occupies.
  -> [Term (S.Syntax Text) (Record '[Range, Category, SourceSpan])] -- ^ The child nodes of the term.
  -> IO (Term (S.Syntax Text) (Record '[Range, Category, SourceSpan])) -- ^ The resulting term, in IO.
termConstructor source sourceSpan name range children
  | name == "ERROR" = withDefaultInfo (S.Error children)
  | otherwise = withDefaultInfo $ case (name, children) of
    ("assignment", [ identifier, value ]) -> S.Assignment identifier value
    ("comment", _) -> S.Comment . toText $ slice range source
    (_, []) -> S.Leaf . toText $ slice range source
    _  -> S.Indexed children
  where
    withDefaultInfo syntax = do
      sourceSpan' <- sourceSpan
      pure $! cofree ((range .: categoryForRubyName name .: sourceSpan' .: RNil) :< syntax)

categoryForRubyName :: Text -> Category
categoryForRubyName = \case
  "assignment" -> Assignment
  "boolean" -> Boolean
  "comment" -> Comment
  "ERROR" -> Error
  "float" -> NumberLiteral
  "identifier" -> Identifier
  "integer" -> IntegerLiteral
  "interpolation" -> Interpolation
  "program" -> Program
  "string" -> StringLiteral
  "symbol" -> SymbolLiteral
  s -> Other s
