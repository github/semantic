{-# LANGUAGE DataKinds, GADTs #-}
module Language.C where

import Data.Record
import Info
import Prologue
import Source
import qualified Syntax as S
import Term

termConstructor
  :: Source Char -- ^ The source that the term occurs within.
  -> Record '[Range, Category, SourceSpan] -- ^ The proposed annotation for the term.
  -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ The child nodes of the term.
  -> IO [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ All child nodes (included unnamed productions) of the term as 'IO'. Only use this if you need it.
  -> IO (SyntaxTerm Text '[Range, Category, SourceSpan]) -- ^ The resulting term, in IO.
termConstructor source (range :. category :. sourceSpan :. Nil) children _
  | category == Error = withDefaultInfo (S.Error children)
  | otherwise = withDefaultInfo $ case children of
  [] -> S.Leaf . toText $ slice range source
  _ -> S.Indexed children
  where
    withDefaultInfo syntax = pure $! cofree ((range :. category :. sourceSpan :. Nil) :< syntax)

categoryForCProductionName :: Text -> Category
categoryForCProductionName = Other
