{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeOperators #-}
module Renderer
( DiffRenderer(..)
, TermRenderer(..)
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
, identifierAlgebra
, Summaries(..)
, File(..)
) where

import Data.Aeson (Value, (.=))
import qualified Data.Map as Map
import Data.Syntax.Algebra (RAlgebra)
import Diff (SyntaxDiff)
import Info (DefaultFields)
import Prologue
import Renderer.JSON as R
import Renderer.Patch as R
import Renderer.SExpression as R
import Renderer.TOC as R
import Syntax as S
import Term (SyntaxTerm)

-- | Specification of renderers for diffs, producing output in the parameter type.
data DiffRenderer output where
  -- | Render to git-diff-compatible textual output.
  PatchDiffRenderer :: DiffRenderer File
  -- | Compute a table of contents for the diff & encode it as JSON.
  ToCDiffRenderer :: DiffRenderer Summaries
  -- | Render to JSON with the format documented in docs/json-format.md
  JSONDiffRenderer :: DiffRenderer (Map.Map Text Value)
  -- | Render to a 'ByteString' formatted as nested s-expressions with patches indicated.
  SExpressionDiffRenderer :: DiffRenderer ByteString
  -- | “Render” by returning the computed 'SyntaxDiff'. This renderer is not surfaced in the command-line interface, and is intended strictly for tests. Further, as it cannot render à la carte terms, it should be regarded as a (very) short-term hack until such time as we have a better idea for TOCSpec.hs.
  IdentityDiffRenderer :: DiffRenderer (Maybe (SyntaxDiff Text (Maybe Declaration ': DefaultFields)))

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
  -- | “Render” by returning the computed 'SyntaxTerm'. This renderer is not surfaced in the command-line interface, and is intended strictly for tests. Further, as it cannot render à la carte terms, it should be regarded as a (very) short-term hack until such time as we have a better idea for SemanticSpec.hs.
  IdentityTermRenderer :: TermRenderer (Maybe (SyntaxTerm Text DefaultFields))

deriving instance Eq (TermRenderer output)
deriving instance Show (TermRenderer output)


identifierAlgebra :: RAlgebra (CofreeF (Syntax Text) a) (Cofree (Syntax Text) a) (Maybe Identifier)
identifierAlgebra (_ :< syntax) = case syntax of
  S.Assignment f _ -> identifier f
  S.Class f _ _ -> identifier f
  S.Export f _ -> f >>= identifier
  S.Function f _ _ -> identifier f
  S.FunctionCall f _ _ -> identifier f
  S.Import f _ -> identifier f
  S.Method _ f _ _ _ -> identifier f
  S.MethodCall _ f _ _ -> identifier f
  S.Module f _ -> identifier f
  S.OperatorAssignment f _ -> identifier f
  S.SubscriptAccess f _  -> identifier f
  S.TypeDecl f _ -> identifier f
  S.VarAssignment f _ -> asum $ identifier <$> f
  _ -> Nothing
  where identifier = fmap Identifier . extractLeafValue . unwrap . fst

newtype Identifier = Identifier Text
  deriving (Eq, NFData, Show)

instance ToJSONFields Identifier where
  toJSONFields (Identifier i) = ["identifier" .= i]
