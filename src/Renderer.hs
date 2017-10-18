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
, renderToTags
, HasDeclaration
, declarationAlgebra
, syntaxDeclarationAlgebra
, Summaries(..)
, File(..)
) where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Output
import Data.Text (Text)
import Renderer.JSON as R
import Renderer.Patch as R
import Renderer.SExpression as R
import Renderer.TOC as R

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
  -- | Render to a list of tags.
  TagsTermRenderer :: TermRenderer [Value]

deriving instance Eq (TermRenderer output)
deriving instance Show (TermRenderer output)


-- | Abstraction of some renderer to some 'Monoid'al output which can be serialized to a 'ByteString'.
--
--   This type abstracts the type indices of 'DiffRenderer' and 'TermRenderer' s.t. multiple renderers can be present in a single list, alternation, etc., while retaining the ability to render and serialize. (Without 'SomeRenderer', the different output types of individual term/diff renderers prevent them from being used in a homogeneously typed setting.)
data SomeRenderer f where
  SomeRenderer :: (Output output, Show (f output)) => f output -> SomeRenderer f

deriving instance Show (SomeRenderer f)
