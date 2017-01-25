{-# LANGUAGE DataKinds #-}
module Language.C where

import Info
import Prologue
import Source
import qualified Syntax as S
import Term

termAssignment
  :: Source Char -- ^ The source of the term.
  -> Category -- ^ The category for the term.
  -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ The child nodes of the term.
  -> Maybe (S.Syntax Text (SyntaxTerm Text '[Range, Category, SourceSpan])) -- ^ The resulting term, in IO.
termAssignment _ _ _ = Nothing


categoryForCProductionName :: Text -> Category
categoryForCProductionName = Other
