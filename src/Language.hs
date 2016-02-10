module Language where

import Data.Text

-- | A programming language.
data Language =
    C
    | JavaScript

-- | Returns a Language based on the file extension (including the ".").
languageForType :: Text -> Maybe Language
languageForType mediaType = case mediaType of
    ".h" -> Just C
    ".c" -> Just C
    ".js" -> Just JavaScript
    _ -> Nothing
