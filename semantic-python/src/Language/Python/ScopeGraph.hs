module Language.Python.ScopeGraph () where

import           Control.Effect.Reader
import           Control.Effect.Writer
import           Source.Loc
-- import           Source.Range
import           Source.Source as Source
import           Data.ScopeGraph (ScopeGraph, Info)
