{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Tags.Tag (Tag (..), UTF16CodeUnitSpan(..), OneIndexedSpan(..)) where

import Data.Text (Text)
import qualified Proto.Semantic as P
import Source.Loc

-- | A 0-indxed Span where the column offset units are utf-16 code units (2
-- bytes), suitable for the LSP (Language Server Protocol) specification.
newtype UTF16CodeUnitSpan = UTF16CodeUnitSpan { unUTF16CodeUnitSpan :: Span }
  deriving (Eq, Show)

-- A 1-indexed Span where the units are bytes.
newtype OneIndexedSpan = OneIndexedSpan { unOneIndexedSpan :: Span }
  deriving (Eq, Show)

data Tag
  = Tag
      { tagName :: Text,
        tagSyntaxType :: P.SyntaxType,
        tagNodeType :: P.NodeType,
        tagByteRange :: Range,
        tagOneIndexedSpan :: OneIndexedSpan,
        tagLine :: Text,
        tagUTF16CodeUnitSpan :: UTF16CodeUnitSpan
      }
  deriving (Eq, Show)
