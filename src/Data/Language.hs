{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, KindSignatures, LambdaCase, OverloadedStrings #-}
module Data.Language
  ( Language (..)
  , SLanguage (..)
  , extensionsForLanguage
  , knownLanguage
  , languageForFilePath
  , pathIsMinified
  , supportedExts
  , codeNavLanguages
  , textToLanguage
  , languageToText
  , PerLanguageModes(..)
  , defaultLanguageModes
  , LanguageMode(..)
  ) where

import           Data.Aeson
import qualified Data.Languages as Lingo
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
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
    deriving (Eq, Generic, Ord, Read, Show, Bounded, Hashable, ToJSON, Enum)

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
    pure $ textToLanguage l

-- | Predicate failing on 'Unknown' and passing in all other cases.
knownLanguage :: Language -> Bool
knownLanguage = (/= Unknown)

extensionsForLanguage :: Language -> [String]
extensionsForLanguage language = T.unpack <$> maybe mempty Lingo.languageExtensions (Map.lookup (languageToText language) Lingo.languages)

-- | Return a language based on a FilePath's extension.
languageForFilePath :: FilePath -> Language
languageForFilePath path =
  let spurious lang = lang `elem` [ "Hack" -- .php files
                                  , "GCC Machine Description" -- .md files
                                  , "XML" -- .tsx files
                                  ]
      allResults = Lingo.languageName <$> Lingo.languagesForPath path
  in case filter (not . spurious) allResults of
    [result] -> textToLanguage result
    _        -> Unknown

supportedExts :: [String]
supportedExts = foldr append mempty supportedLanguages
  where
    append (Just l) b = fmap T.unpack (Lingo.languageExtensions l) <> b
    append Nothing  b = b
    supportedLanguages = fmap lookup (languageToText <$> codeNavLanguages)
    lookup k = Map.lookup k Lingo.languages

codeNavLanguages :: [Language]
codeNavLanguages = [Go, Java, Ruby, Python, JavaScript, TypeScript, PHP]

pathIsMinified :: FilePath -> Bool
pathIsMinified = isExtensionOf ".min.js"

languageToText :: Language -> T.Text
languageToText = \case
  Unknown -> "Unknown"
  Go -> "Go"
  Haskell -> "Haskell"
  Java -> "Java"
  JavaScript -> "JavaScript"
  JSON -> "JSON"
  JSX -> "JSX"
  Markdown -> "Markdown"
  Python -> "Python"
  Ruby -> "Ruby"
  TypeScript -> "TypeScript"
  TSX -> "TSX"
  PHP -> "PHP"

textToLanguage :: T.Text -> Language
textToLanguage = \case
  "Go" -> Go
  "Haskell" -> Haskell
  "Java" -> Java
  "JavaScript" -> JavaScript
  "JSON" -> JSON
  "JSX" -> JSX
  "Markdown" -> Markdown
  "Python" -> Python
  "Ruby" -> Ruby
  "TypeScript" -> TypeScript
  "TSX" -> TSX
  "PHP" -> PHP
  _ -> Unknown


newtype PerLanguageModes = PerLanguageModes
  { pythonMode :: LanguageMode
  }
  deriving (Eq, Ord, Show)

defaultLanguageModes :: PerLanguageModes
defaultLanguageModes = PerLanguageModes
  { pythonMode = ALaCarte
  }

data LanguageMode
  = ALaCarte
  | Precise
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
