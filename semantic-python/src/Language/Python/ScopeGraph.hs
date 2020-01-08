module Language.Python.ScopeGraph (ToScopeGraph(..)) where

import           Control.Effect.Reader
import           Control.Effect.Writer
import           Source.Loc
-- import           Source.Range
import           Source.Source as Source
import           Data.ScopeGraph (ScopeGraph, Info)

class ToScopeGraph t where
  scopeGraph :: ( Has (Reader Source) sig m, Has (Writer (ScopeGraph Info)) sig m) => t Loc -> m ()
