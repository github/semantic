{-# LANGUAGE DeriveAnyClass, DeriveGeneric, LambdaCase #-}
module Data.Language where

import Data.Aeson
import Prologue
import Proto3.Suite

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
    deriving (Eq, Generic, Ord, Read, Show, Bounded, Hashable, ToJSON, Named, Enum, Finite, MessageField)

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
