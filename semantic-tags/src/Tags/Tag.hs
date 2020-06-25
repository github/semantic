module Tags.Tag (Tag (..), UTF16CodeUnitSpan, OneIndexedSpan) where

import Data.Text (Text)
import qualified Proto.Semantic as P
import Source.Loc

-- | A 0-indxed Span where the column offset units are utf-16 code units (2
-- bytes), suitable for the LSP (Language Server Protocol) specification.
type UTF16CodeUnitSpan = Span

-- A 1-indexed Span where the units are bytes.
type OneIndexedSpan = Span

data Tag
  = Tag
      { tagName :: Text,
        tagSyntaxType :: P.SyntaxType,
        tagNodeType :: P.NodeType,
        tagSpan :: OneIndexedSpan,
        tagLine :: Text,
        tagLspSpan :: UTF16CodeUnitSpan
      }
  deriving (Eq, Show)
