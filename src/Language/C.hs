{-# LANGUAGE DataKinds #-}
module Language.C where

import Data.Record
import Info
import Prologue
import Source
import qualified Syntax as S
import Term

termConstructor
  :: Source Char -- ^ The source that the term occurs within.
  -> SourceSpan -- ^ The span that the term occupies.
  -> Text -- ^ The name of the production for this node.
  -> Range -- ^ The character range that the term occupies.
  -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ The child nodes of the term.
  -> IO [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ All child nodes (included unnamed productions) of the term as 'IO'. Only use this if you need it.
  -> IO (SyntaxTerm Text '[Range, Category, SourceSpan]) -- ^ The resulting term, in IO.
termConstructor source sourceSpan name range children _
  | name == "ERROR" = withDefaultInfo (S.Error children)
  | otherwise = withDefaultInfo $ case (name, children) of
  (_, []) -> S.Leaf . toText $ slice range source
  _ -> S.Indexed children
  where
    withDefaultInfo syntax = pure $! cofree ((range .: categoryForCProductionName name .: sourceSpan .: Nil) :< syntax)

categoryForCProductionName :: Text -> Category
categoryForCProductionName name = Other name
