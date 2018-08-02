{-# LANGUAGE DeriveAnyClass, DeriveGeneric, KindSignatures, LambdaCase #-}
module Data.Language
  ( Language (..)
  , ensureLanguage
  , extensionsForLanguage
  , knownLanguage
  , languageForFilePath
  , languageForType
  ) where

import           Data.Aeson
import           Data.Char (toUpper)
import           Data.String
import qualified Data.Text as T
import           Prologue
import           Proto3.Suite
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
    deriving (Eq, Generic, Ord, Read, Show, Bounded, Hashable, ToJSON, Named, Enum, MessageField)

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


-- This ensures that the protobuf file is generated with ALL_CAPS_NAMES.
instance Finite Language where
  enumerate _ = fmap go [Unknown ..] where
    go x = (fromString (fmap toUpper (show x)), fromEnum x)

instance FromJSON Language where
  parseJSON = withText "Language" $ \l -> pure $ case T.toLower l of
    "go"         -> Go
    "haskell"    -> Haskell
    "java"       -> Java
    "javascript" -> JavaScript
    "json"       -> JSON
    "jsx"        -> JSX
    "markdown"   -> Markdown
    "python"     -> Python
    "ruby"       -> Ruby
    "typescript" -> TypeScript
    "php"        -> PHP
    _            -> Unknown

-- | Predicate failing on 'Unknown' and passing in all other cases.
knownLanguage :: Language -> Bool
knownLanguage = (/= Unknown)

-- | Returns 'Nothing' when passed 'Unknown'.
ensureLanguage :: Language -> Maybe Language
ensureLanguage Unknown = Nothing
ensureLanguage x       = Just x

-- | Defaults to 'Unknown'.
instance HasDefault Language where def = Unknown

-- | Piggybacks on top of the 'Enumerated' instance, as the generated code would.
-- This instance will get easier when we have DerivingVia.
instance Primitive Language where
  primType _ = primType (Proxy @(Enumerated Language))
  encodePrimitive f = encodePrimitive f . Enumerated . Right
  decodePrimitive   = decodePrimitive >>= \case
    (Enumerated (Right r)) -> pure r
    other                  -> Prelude.fail ("Language decodeMessageField: unexpected value" <> show other)

-- | Returns a Language based on the file extension (including the ".").
languageForType :: String -> Language
languageForType mediaType = case mediaType of
    ".java" -> Java
    ".json" -> JSON
    ".hs"   -> Haskell
    ".md"   -> Markdown
    ".rb"   -> Ruby
    ".go"   -> Go
    ".js"   -> JavaScript
    ".ts"   -> TypeScript
    ".tsx"  -> TypeScript
    ".jsx"  -> JSX
    ".py"   -> Python
    ".php"  -> PHP
    ".phpt" -> PHP
    _       -> Unknown

extensionsForLanguage :: Language -> [String]
extensionsForLanguage language = case language of
  Go         -> [".go"]
  Haskell    -> [".hs"]
  JavaScript -> [".js"]
  PHP        -> [".php"]
  Python     -> [".py"]
  Ruby       -> [".rb"]
  TypeScript -> [".ts", ".tsx", ".d.tsx"]
  _          -> []

-- | Return a language based on a FilePath's extension, or Nothing if extension is not found or not supported.
languageForFilePath :: FilePath -> Language
languageForFilePath = languageForType . takeExtension
