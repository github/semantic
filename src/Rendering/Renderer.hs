{-# LANGUAGE DataKinds, GADTs, MultiParamTypeClasses, StandaloneDeriving, TypeOperators #-}
module Rendering.Renderer
( DiffRenderer(..)
, renderJSONDiff
, renderJSONAdjDiff
, renderJSONTerm
, renderJSONAdjTerm
, renderJSONAST
, renderToCDiff
, renderRPCToCDiff
, renderToCTerm
, renderSymbolTerms
, renderTreeGraph
, renderJSONError
, renderJSONSymbolError
, renderJSONDiffError
, renderJSONSummaryError
, Summaries(..)
, TOCSummary(..)
) where

import Data.ByteString.Builder
import Data.Graph
import Data.Graph.DiffVertex
import Rendering.Graph as R
import Rendering.JSON as R
import Rendering.TOC as R

-- | Specification of renderers for diffs, producing output in the parameter type.
data DiffRenderer output where
  -- | Render to JSON as an adjacency list.
  JSONGraphDiffRenderer :: DiffRenderer (JSON "diffs" SomeJSON)
  -- | Render to a 'ByteString' formatted as nested s-expressions with patches indicated.
  SExpressionDiffRenderer :: DiffRenderer Builder
  -- | Render to a 'ByteString' formatted as a DOT description of the diff.
  DOTDiffRenderer :: DiffRenderer (Graph DiffVertex)
  -- | Render to a 'ByteString' formatted using the 'Show' instance.
  ShowDiffRenderer :: DiffRenderer Builder

deriving instance Eq (DiffRenderer output)
deriving instance Show (DiffRenderer output)
