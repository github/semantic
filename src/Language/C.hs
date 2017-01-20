{-# LANGUAGE DataKinds, GADTs #-}
module Language.C where

import Data.Record
import Info
import Prologue
import Source
import qualified Syntax as S
import Term

termAssignment
  :: Source Char -- ^ The source of the term.
  -> Record '[Range, Category, SourceSpan] -- ^ The proposed annotation for the term.
  -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ The child nodes of the term.
  -> Maybe (S.Syntax Text (SyntaxTerm Text '[Range, Category, SourceSpan])) -- ^ The resulting term, in IO.
termAssignment _ _ _ = Nothing


categoryForCProductionName :: Text -> Category
categoryForCProductionName = Other
