{-# LANGUAGE DataKinds #-}
module Language.C where

import Data.Record
import Info
import Prologue
import Source
import SourceSpan
import qualified Syntax as S
import Term

termConstructor
  :: Source Char -- ^ The source that the term occurs within.
  -> IO SourceSpan -- ^ The span that the term occupies. This is passed in 'IO' to guarantee some access constraints & encourage its use only when needed (improving performance).
  -> Text -- ^ The name of the production for this node.
  -> Range -- ^ The character range that the term occupies.
  -> [Term Text (Record '[Range, Category])] -- ^ The child nodes of the term.
  -> IO (Term Text (Record '[Range, Category])) -- ^ The resulting term, in IO.
termConstructor source sourceSpan name range children
  | name == "ERROR" = sourceSpan >>= withDefaultInfo . (`S.Error` children)
  | otherwise = withDefaultInfo $ case (name, children) of
  (_, []) -> S.Leaf . toText $ slice range source
  _ -> S.Indexed children
  where withDefaultInfo syntax = pure $! cofree ((range .: categoryForCProductionName name .: RNil) :< syntax)

categoryForCProductionName :: Text -> Category
categoryForCProductionName name = case name of
  _ -> Other name
