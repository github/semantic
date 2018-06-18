{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Rendering.Symbol
( renderToSymbols
, SymbolFields(..)
, defaultSymbolFields
, parseSymbolFields
) where

import Prologue
import Analysis.Declaration
import Data.Aeson
import Data.Blob
import Data.Language (ensureLanguage)
import Data.Record
import Data.Span
import Data.List.Split (splitWhen)
import Data.Term
import qualified Data.Text as T
import Rendering.TOC


-- | Render a 'Term' to a list of symbols (See 'Symbol').
renderToSymbols :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => SymbolFields -> Blob -> Term f (Record fields) -> [Value]
renderToSymbols fields Blob{..} term = [toJSON (termToC fields blobPath term)]
  where
    termToC :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => SymbolFields -> FilePath -> Term f (Record fields) -> File
    termToC fields path = File (T.pack path) (T.pack (show blobLanguage)) . mapMaybe (symbolSummary fields path "unchanged") . termTableOfContentsBy declaration

-- | Construct a 'Symbol' from a node annotation and a change type label.
symbolSummary :: (HasField fields (Maybe Declaration), HasField fields Span) => SymbolFields -> FilePath -> T.Text -> Record fields -> Maybe Symbol
symbolSummary SymbolFields{..} path _ record = case getDeclaration record of
  Just ErrorDeclaration{} -> Nothing
  Just declaration -> Just Symbol
    { symbolName = when symbolFieldsName (declarationIdentifier declaration)
    , symbolPath = when symbolFieldsPath (T.pack path)
    , symbolLang = join (when symbolFieldsLang (T.pack . show <$> ensureLanguage (declarationLanguage declaration)))
    , symbolKind = when symbolFieldsKind (toCategoryName declaration)
    , symbolLine = when symbolFieldsLine (declarationText declaration)
    , symbolSpan = when symbolFieldsSpan (getField record)
    }
  _ -> Nothing

data File = File
  { filePath :: T.Text
  , fileLanguage :: T.Text
  , fileSymbols :: [Symbol]
  } deriving (Generic, Eq, Show)

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
  } deriving (Generic, Eq, Show)

instance ToJSON Symbol where
  toJSON Symbol{..} = objectWithoutNulls
    [ "symbol" .= symbolName
    , "path" .= symbolPath
    , "language" .= symbolLang
    , "kind" .= symbolKind
    , "line" .= symbolLine
    , "span" .= symbolSpan ]
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
  }
  deriving (Eq, Show)

defaultSymbolFields :: SymbolFields
defaultSymbolFields = SymbolFields True False False True False True

defaultTagSymbolFields :: SymbolFields
defaultTagSymbolFields = SymbolFields True True True True True True

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
    }
