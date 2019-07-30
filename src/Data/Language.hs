{-# LANGUAGE DeriveAnyClass, DeriveGeneric, KindSignatures #-}
module Data.Language
  ( Language (..)
  , SLanguage (..)
  , extensionsForLanguage
  , parseLanguage
  , knownLanguage
  , languageForFilePath
  , pathIsMinified
  , supportedExts
  , codeNavLanguages
  ) where

import           Data.Aeson
import qualified Data.Languages as Lingo
import qualified Data.Text as T
import           Prologue
import           System.FilePath.Posix

-- | The various languages we support.
-- Please do not reorder any of the field names: the current implementation of 'Primitive'
-- delegates to the auto-generated 'Enum' instance.
data Language
    = Unknown
    | Go
    | Haskell
    | Java
    | JavaScript
    | JSON
    | JSX
    | Markdown
    | Python
    | Ruby
    | TypeScript
    | PHP
    | TSX
    deriving (Eq, Generic, Ord, Read, Show, Bounded, Hashable, ToJSON, Enum, NFData)

class SLanguage (lang :: Language) where
  reflect :: proxy lang -> Language

instance SLanguage 'Unknown where
  reflect _ = Unknown

instance SLanguage 'Go where
  reflect _ = Go

instance SLanguage 'Haskell where
  reflect _ = Haskell

instance SLanguage 'Java where
  reflect _ = Java

instance SLanguage 'JavaScript where
  reflect _ = JavaScript

instance SLanguage 'JSON where
  reflect _ = JSON

instance SLanguage 'JSX where
  reflect _ = JSX

instance SLanguage 'Markdown where
  reflect _ = Markdown

instance SLanguage 'Python where
  reflect _ = Python

instance SLanguage 'Ruby where
  reflect _ = Ruby

instance SLanguage 'TypeScript where
  reflect _ = TypeScript

instance SLanguage 'PHP where
  reflect _ = PHP

instance FromJSON Language where
  parseJSON = withText "Language" $ \l ->
    pure $ fromMaybe Unknown (parseLanguage l)

parseLanguage :: Text -> Maybe Language
parseLanguage l = case T.toLower l of
  "go"         -> Just Go
  "haskell"    -> Just Haskell
  "java"       -> Just Java
  "javascript" -> Just JavaScript
  "json"       -> Just JSON
  "jsx"        -> Just JSX
  "markdown"   -> Just Markdown
  "python"     -> Just Python
  "ruby"       -> Just Ruby
  "typescript" -> Just TypeScript
  "php"        -> Just PHP
  _            -> Nothing

-- | Predicate failing on 'Unknown' and passing in all other cases.
knownLanguage :: Language -> Bool
knownLanguage = (/= Unknown)

extensionsForLanguage :: Language -> [String]
extensionsForLanguage language = case language of
  Go         -> [".go"]
  Haskell    -> [".hs"]
  JavaScript -> [".js", ".mjs"]
  PHP        -> [".php", ".phpt"]
  Python     -> [".py"]
  Ruby       -> [".rb"]
  TypeScript -> [".ts"]
  TSX        -> [".tsx", ".d.tsx"]
  JSX        -> [".jsx"]
  _          -> []

-- | Return a language based on a FilePath's extension.
languageForFilePath :: FilePath -> Language
languageForFilePath path = case Lingo.languageName <$> Lingo.languageForPath path of
  Just "Go" -> Go
  Just "Haskell" -> Haskell
  Just "Java" -> Java
  Just "JavaScript" -> JavaScript
  Just "JSON" -> JSON
  Just "JSX" -> JSX
  Just "Markdown" -> Markdown
  Just "PHP" -> PHP
  Just "Python" -> Python
  Just "Ruby" -> Ruby
  Just "TSX" -> TSX
  Just "TypeScript" -> TypeScript
  _ -> Unknown

supportedExts :: [String]
supportedExts = [".go", ".py", ".rb", ".js", ".mjs", ".ts", ".php", ".phpt"]

codeNavLanguages :: [Language]
codeNavLanguages = [Go, Ruby, Python, JavaScript, TypeScript, PHP]

pathIsMinified :: FilePath -> Bool
pathIsMinified = isExtensionOf ".min.js"
