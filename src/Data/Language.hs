{-# LANGUAGE DeriveGeneric, DeriveAnyClass, LambdaCase #-}
module Data.Language where

import Prologue
import Data.Aeson
import Debug.Trace
import Proto3.Suite

-- | A programming language.
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
    deriving (Eq, Generic, Ord, Read, Show, Bounded, ToJSON, Named, Enum, Finite, MessageField)

knownLanguage :: Language -> Bool
knownLanguage = (/= Unknown)

ensureLanguage :: Language -> Maybe Language
ensureLanguage Unknown = Nothing
ensureLanguage x = Just x

-- | Defaults to 'PACKAGE'.
instance HasDefault Language where def = Unknown

-- | Piggybacks on top of the 'Enumerated' instance, as the generated code would.
-- This instance will get easier when we have DerivingVia.
instance Primitive Language where
  primType _ = primType (Proxy @(Enumerated Language))
  encodePrimitive f = encodePrimitive f . Enumerated . Right
  decodePrimitive   = decodePrimitive >>= \case
    (Enumerated (Right r)) -> pure (succ r)
    other                  -> Prelude.fail ("Language decodeMessageField: unexpected value" <> show other)

-- | Returns a Language based on the file extension (including the ".").
languageForType :: String -> Language
languageForType mediaType = case mediaType of
    ".java" -> Java
    ".json" -> JSON
    ".hs" -> Haskell
    ".md" -> Markdown
    ".rb" -> Ruby
    ".go" -> Go
    ".js" -> JavaScript
    ".ts" -> TypeScript
    ".tsx" -> TypeScript
    ".jsx" -> JSX
    ".py" -> Python
    ".php" -> PHP
    ".phpt" -> PHP
    _ -> Unknown

extensionsForLanguage :: Language -> [String]
extensionsForLanguage language = case language of
  Go -> [".go"]
  Haskell -> [".hs"]
  JavaScript -> [".js"]
  PHP -> [".php"]
  Python -> [".py"]
  Ruby -> [".rb"]
  TypeScript -> [".ts", ".tsx", ".d.tsx"]
  _ -> []
