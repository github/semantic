module Language where

import Prologue

-- | A programming language.
data Language =
    C
    | CoffeeScript
    | CPlusPlus
    | CSharp
    | CSS
    | Haskell
    | HTML
    | Java
    | JavaScript
    | ObjectiveC
    | Perl
    | PHP
    | Python
    | R
    | Ruby
    | Swift
    deriving (Show)

-- | Returns a Language based on the file extension (including the ".").
languageForType :: Text -> Maybe Language
languageForType mediaType = case mediaType of
    ".h" -> Just C
    ".c" -> Just C
    ".js" -> Just JavaScript
    ".rb" -> Just Ruby
    _ -> Nothing
