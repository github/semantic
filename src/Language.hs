{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass #-}
module Language where

import Control.DeepSeq
import Data.Aeson
import GHC.Generics

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
    deriving (Show, Eq, Read, Generic, NFData, ToJSON)

-- | Returns a Language based on the file extension (including the ".").
languageForType :: String -> Maybe Language
languageForType mediaType = case mediaType of
    ".json" -> Just JSON
    ".md" -> Just Markdown
    ".rb" -> Just Ruby
    ".go" -> Just Language.Go
    ".js" -> Just TypeScript
    ".ts" -> Just TypeScript
    ".tsx" -> Just TypeScript
    ".jsx" -> Just JSX
    ".py" -> Just Python
    _ -> Nothing
