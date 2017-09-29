{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeOperators #-}
module Renderer
( DiffRenderer(..)
, TermRenderer(..)
, SomeRenderer(..)
, renderPatch
, renderSExpressionDiff
, renderSExpressionTerm
, renderJSONDiff
, renderJSONTerm
, renderToCDiff
, renderToCTerm
, declarationAlgebra
, markupSectionAlgebra
, syntaxDeclarationAlgebra
, Summaries(..)
, File(..)
) where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Diff
import qualified Data.Map as Map
import Data.Output
import Data.Record
import Data.Text (Text)
import Info (DefaultFields)
import Renderer.JSON as R
import Renderer.Patch as R
import Renderer.SExpression as R
import Renderer.TOC as R
import Syntax as S

-- | Specification of renderers for diffs, producing output in the parameter type.
data DiffRenderer output where
  -- | Render to git-diff-compatible textual output.
  PatchDiffRenderer :: DiffRenderer File
  -- | Compute a table of contents for the diff & encode it as JSON.
  OldToCDiffRenderer :: DiffRenderer Summaries
  -- | Compute a table of contents for the diff & encode it as JSON (uses the new Assignment parse tree parser).
  ToCDiffRenderer :: DiffRenderer Summaries
  -- | Render to JSON with the format documented in docs/json-format.md
  JSONDiffRenderer :: DiffRenderer (Map.Map Text Value)
  -- | Render to a 'ByteString' formatted as nested s-expressions with patches indicated.
  SExpressionDiffRenderer :: DiffRenderer ByteString
  -- | “Render” by returning the computed 'Diff'. This renderer is not surfaced in the command-line interface, and is intended strictly for tests. Further, as it cannot render à la carte terms, it should be regarded as a (very) short-term hack until such time as we have a better idea for TOCSpec.hs.
  IdentityDiffRenderer :: DiffRenderer (Maybe (Diff Syntax (Record (Maybe Declaration ': DefaultFields)) (Record (Maybe Declaration ': DefaultFields))))

deriving instance Eq (DiffRenderer output)
deriving instance Show (DiffRenderer output)

-- | Specification of renderers for terms, producing output in the parameter type.
data TermRenderer output where
  -- | Compute a table of contents for the term & encode it as JSON.
  ToCTermRenderer :: TermRenderer Summaries
  -- | Render to JSON with the format documented in docs/json-format.md under “Term.”
  JSONTermRenderer :: TermRenderer [Value]
  -- | Render to a 'ByteString' formatted as nested s-expressions.
  SExpressionTermRenderer :: TermRenderer ByteString

deriving instance Eq (TermRenderer output)
deriving instance Show (TermRenderer output)


-- | Abstraction of some renderer to some 'Monoid'al output which can be serialized to a 'ByteString'.
--
--   This type abstracts the type indices of 'DiffRenderer' and 'TermRenderer' s.t. multiple renderers can be present in a single list, alternation, etc., while retaining the ability to render and serialize. (Without 'SomeRenderer', the different output types of individual term/diff renderers prevent them from being used in a homogeneously typed setting.)
data SomeRenderer f where
  SomeRenderer :: (Output output, Show (f output)) => f output -> SomeRenderer f

deriving instance Show (SomeRenderer f)
