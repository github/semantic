module Tags.Tag (Tag (..)) where

import Data.Text (Text)
import Proto.Semantic as P
import Source.Loc

data Tag
  = Tag
      { name :: Text,
        kind :: P.SyntaxType,
        loc :: Loc,
        line :: Text,
        docs :: Maybe Text
      }
  deriving (Eq, Show)
