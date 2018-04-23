{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Data.Language where

import Prologue
import Data.Aeson

-- | A programming language.
data Language
    = Go
    | JavaScript
    | JSON
    | JSX
    | Markdown
    | Python
    | Ruby
    | TypeScript
    | PHP
    deriving (Eq, Generic, Ord, Read, Show, ToJSON)

-- | Returns a Language based on the file extension (including the ".").
languageForType :: String -> Maybe Language
languageForType mediaType = case mediaType of
    ".json" -> Just JSON
    ".md" -> Just Markdown
    ".rb" -> Just Ruby
    ".go" -> Just Go
    ".js" -> Just JavaScript
    ".ts" -> Just TypeScript
    ".tsx" -> Just TypeScript
    ".jsx" -> Just JSX
    ".py" -> Just Python
    ".php" -> Just PHP
    ".phpt" -> Just PHP
    _ -> Nothing

extensionsForLanguage :: Maybe Language -> [String]
extensionsForLanguage Nothing         = []
extensionsForLanguage (Just language) = case language of
  Go -> ["go"]
  JavaScript -> ["js"]
  PHP -> ["php"]
  Python -> ["py"]
  Ruby -> ["rb"]
  TypeScript -> ["ts", "tsx", "d.tsx"]
  _ -> []
