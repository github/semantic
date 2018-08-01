{-# LANGUAGE DataKinds, GADTs, MultiParamTypeClasses, StandaloneDeriving, TypeOperators #-}
module Rendering.Renderer
( DiffRenderer(..)
, TermRenderer(..)
, renderJSONDiff
, renderJSONTerm
, renderJSONAST
, renderToCDiff
, renderRPCToCDiff
, renderToCTerm
, renderSymbolTerms
, renderToSymbols
, renderTreeGraph
, Summaries(..)
, TOCSummary(..)
, SymbolFields(..)
, defaultSymbolFields
, parseSymbolFields
) where

import Data.ByteString.Builder
import Data.Graph
import Rendering.Graph as R
import Rendering.JSON as R
import Rendering.Symbol as R
import Rendering.TOC as R

-- | Specification of renderers for diffs, producing output in the parameter type.
data DiffRenderer output where
  -- | Compute a table of contents for the diff & encode it as JSON.
  ToCDiffRenderer :: DiffRenderer Summaries
  -- | Render to JSON with the format documented in docs/json-format.md
  JSONDiffRenderer :: DiffRenderer (JSON "diffs" SomeJSON)
  -- | Render to a 'ByteString' formatted as nested s-expressions with patches indicated.
  SExpressionDiffRenderer :: DiffRenderer Builder
  -- | Render to a 'ByteString' formatted as a DOT description of the diff.
  DOTDiffRenderer :: DiffRenderer (Graph (Vertex DiffTag))
  -- | Render to a 'ByteString' formatted using the 'Show' instance.
  ShowDiffRenderer :: DiffRenderer Builder

deriving instance Eq (DiffRenderer output)
deriving instance Show (DiffRenderer output)

-- | Specification of renderers for terms, producing output in the parameter type.
data TermRenderer output where
  -- | Render to JSON with the format documented in docs/json-format.md under “Term.”
  JSONTermRenderer :: TermRenderer (JSON "trees" SomeJSON)
  -- | Render to a 'ByteString' formatted as nested s-expressions.
  SExpressionTermRenderer :: TermRenderer Builder
  -- | Render to a list of symbols.
  SymbolsTermRenderer :: SymbolFields -> TermRenderer (JSON "files" SomeJSON)
  -- | Render to a 'ByteString' formatted as a DOT description of the term.
  DOTTermRenderer :: TermRenderer (Graph (Vertex ()))
  -- | Render to a 'ByteString' formatted using the 'Show' instance.
  ShowTermRenderer :: TermRenderer Builder

deriving instance Eq (TermRenderer output)
deriving instance Show (TermRenderer output)
