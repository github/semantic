module Tags.Tag (Tag (..)) where

import Data.Text (Text)
import qualified Proto.Semantic as P
import Source.Loc

data Tag
  = Tag
      { tagName :: Text,
        tagSyntaxType :: P.SyntaxType,
        tagNodeType :: P.NodeType,
        tagLoc :: Loc,
        tagLine :: Text,
        tagDocs :: Maybe Text,
        tagLspSpan :: Span
      }
  deriving (Eq, Show)
