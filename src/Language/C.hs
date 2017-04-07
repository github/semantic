{-# LANGUAGE DataKinds #-}
module Language.C where

import Info
import Prologue
import Source
import qualified Syntax as S
import Term

termAssignment
  :: Source -- ^ The source of the term.
  -> Category -- ^ The category for the term.
  -> [ SyntaxTerm Text DefaultFields ] -- ^ The child nodes of the term.
  -> Maybe (S.Syntax Text (SyntaxTerm Text DefaultFields)) -- ^ The resulting term, in Maybe.
termAssignment _ _ _ = Nothing


categoryForCProductionName :: Text -> Category
categoryForCProductionName = Other
