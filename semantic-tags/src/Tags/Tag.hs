module Tags.Tag (Tag (..)) where

import Data.Text (Text)
import Proto.Semantic as P
import Source.Loc

data Tag
  = Tag
      { tagName :: Text,
        tagSyntaxType :: P.SyntaxType,
        tagNodeType :: P.NodeType,
        tagLoc :: Loc,
        tagLine :: Text,
        tagDocs :: Maybe Text
      }
  deriving (Eq, Show)
