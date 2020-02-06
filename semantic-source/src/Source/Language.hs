{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Source.Language
  ( Language (..)
  , SLanguage (..)
  , extensionsForLanguage
  , knownLanguage
  , forPath
  , textToLanguage
  , languageToText
  ) where

import           Data.Aeson
import           Data.Hashable (Hashable)
import qualified Data.Languages as Lingo
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified System.Path as Path
import qualified System.Path.PartClass as Path.PartClass

-- | The various languages we support.
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

-- | Reifies a proxied type-level 'Language' to a value.
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

forPath :: Path.PartClass.AbsRel ar => Path.File ar -> Language
forPath path =
  let spurious lang = lang `elem` [ "Hack" -- .php files
                                  , "GCC Machine Description" -- .md files
                                  , "XML" -- .tsx files
                                  ]
      allResults = Lingo.languageName <$> Lingo.languagesForPath (Path.toString path)
  in case filter (not . spurious) allResults of
    [result] -> textToLanguage result
    _        -> Unknown

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
