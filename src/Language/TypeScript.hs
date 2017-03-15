{-# LANGUAGE DataKinds #-}
module Language.JavaScript where

import Info
import Prologue
import Source
import Language
import qualified Syntax as S
import Term

termAssignment
  :: Source -- ^ The source of the term.
  -> Category -- ^ The category for the term.
  -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ The child nodes of the term.
  -> Maybe (S.Syntax Text (SyntaxTerm Text '[Range, Category, SourceSpan])) -- ^ The resulting term, in Maybe.
termAssignment _ category children
