{-# LANGUAGE RankNTypes, DerivingStrategies, DeriveAnyClass, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Rendering.Symbol
( renderToSymbols
, File(..)
, Symbol(..)
, SymbolFields(..)
, defaultSymbolFields
, parseSymbolFields
) where

import Prologue hiding (when)

import           Data.Aeson
import           Data.Blob
import           Data.List.Split (splitWhen)
import           Data.Location
import           Data.Term
import qualified Data.Text as T
import           Proto3.Suite
import qualified Proto3.Suite.DotProto as Proto
import qualified Proto3.Wire.Decode as Decode
import qualified Proto3.Wire.Encode as Encode
import           Tags.Taggable
import           Tags.Tagging


-- | Render a 'Term' to a list of symbols (See 'Symbol').
renderToSymbols :: (IsTaggable f) => SymbolFields -> Blob -> Term f Location -> [File]
renderToSymbols fields blob term = either mempty (pure . tagsToFile fields blob) (runTagging blob term)

tagsToFile :: SymbolFields -> Blob -> [Tag] -> File
tagsToFile fields blob@Blob{..} tags = File (T.pack blobPath) (T.pack (show blobLanguage)) (fmap (tagToSymbol fields blob) tags)

-- | Construct a 'Symbol' from a 'Tag'
tagToSymbol :: SymbolFields -> Blob -> Tag -> Symbol
tagToSymbol SymbolFields{..} Blob{..} Tag{..}
  = Symbol
  { symbolName = when symbolFieldsName name
  , symbolPath = when symbolFieldsPath (T.pack blobPath)
  , symbolLang = when symbolFieldsLang (T.pack (show blobLanguage))
  , symbolKind = when symbolFieldsKind kind
  , symbolLine = join (when symbolFieldsLine line)
  , symbolSpan = when symbolFieldsSpan span
  , symbolDocs = join (when symbolFieldsDocs docs)
  }

data File = File
  { filePath :: T.Text
  , fileLanguage :: T.Text
  , fileSymbols :: [Symbol]
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (Named, Message)

instance ToJSON File where
  toJSON File{..} = object [ "path" .= filePath
                           , "language" .= fileLanguage
                           , "symbols" .= fileSymbols ]

data Symbol = Symbol
  { symbolName :: Maybe T.Text
  , symbolPath :: Maybe T.Text
  , symbolLang :: Maybe T.Text
  , symbolKind :: Maybe T.Text
  , symbolLine :: Maybe T.Text
  , symbolSpan :: Maybe Span
  , symbolDocs :: Maybe T.Text
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (Named, Message)

instance Named T.Text where
  nameOf _ = "string"

instance Message T.Text where
  encodeMessage = encodeMessageField
  decodeMessage = Decode.at decodeMessageField
  dotProto _ = [ Proto.DotProtoMessageField $ protoType (Proxy @T.Text) ]

instance ToJSON Symbol where
  toJSON Symbol{..} = objectWithoutNulls
    [ "symbol" .= symbolName
    , "path" .= symbolPath
    , "language" .= symbolLang
    , "kind" .= symbolKind
    , "line" .= symbolLine
    , "span" .= symbolSpan
    , "docs" .= symbolDocs ]
    where objectWithoutNulls = object . filter (\(_, v) -> v /= Null)

when :: Bool -> a -> Maybe a
when True a = Just a
when False _ = Nothing

data SymbolFields = SymbolFields
  { symbolFieldsName :: Bool
  , symbolFieldsPath :: Bool
  , symbolFieldsLang :: Bool
  , symbolFieldsKind :: Bool
  , symbolFieldsLine :: Bool
  , symbolFieldsSpan :: Bool
  , symbolFieldsDocs :: Bool
  }
  deriving (Eq, Show)

defaultSymbolFields :: SymbolFields
defaultSymbolFields = SymbolFields True False False True False True True

parseSymbolFields :: String -> SymbolFields
parseSymbolFields arg =
  let fields = splitWhen (== ',') arg in
  SymbolFields
    { symbolFieldsName = "symbol" `elem` fields
    , symbolFieldsPath = "path" `elem` fields
    , symbolFieldsLang = "language" `elem` fields
    , symbolFieldsKind = "kind" `elem` fields
    , symbolFieldsLine = "line" `elem` fields
    , symbolFieldsSpan = "span" `elem` fields
    , symbolFieldsDocs = "docs" `elem` fields
    }
