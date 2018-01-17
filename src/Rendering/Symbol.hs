{-# LANGUAGE DataKinds, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Rendering.Symbol
( renderSymbolTerms
, renderToSymbols
, renderToTags
, SymbolFields(..)
, defaultSymbolFields
) where

import Analysis.Declaration
import Data.Aeson
import Data.Blob
import Data.Maybe (mapMaybe)
import Data.Record
import Data.Span
import Data.Term
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Map as Map
import Rendering.TOC


-- | Render a 'Term' to a ctags like output (See 'Tag').
renderToTags :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => Blob -> Term f (Record fields) -> [Value]
renderToTags Blob{..} = fmap toJSON . termToC blobPath
  where
    termToC :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => FilePath -> Term f (Record fields) -> [Symbol]
    termToC path = mapMaybe (symbolSummary defaultTagSymbolFields path "unchanged") . termTableOfContentsBy declaration


renderSymbolTerms :: [Value] -> Map.Map T.Text Value
renderSymbolTerms = Map.singleton "files" . toJSON

-- | Render a 'Term' to a list of symbols (See 'Symbol').
renderToSymbols :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => SymbolFields -> Blob -> Term f (Record fields) -> [Value]
renderToSymbols fields Blob{..} term = [toJSON (termToC fields blobPath term)]
  where
    termToC :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => SymbolFields -> FilePath -> Term f (Record fields) -> File
    termToC fields path = File (T.pack path) (T.pack . show <$> blobLanguage) . mapMaybe (symbolSummary fields path "unchanged") . termTableOfContentsBy declaration

-- | Construct a 'Symbol' from a node annotation and a change type label.
symbolSummary :: (HasField fields (Maybe Declaration), HasField fields Span) => SymbolFields -> FilePath -> T.Text -> Record fields -> Maybe Symbol
symbolSummary SymbolFields{..} path _ record = case getDeclaration record of
  Just ErrorDeclaration{} -> Nothing
  Just declaration -> Just $ Symbol
    { symbolName = when symbolFieldsShowName (declarationIdentifier declaration)
    , symbolPath = when symbolFieldsShowPath (T.pack path)
    , symbolLang = if symbolFieldsShowLang then (T.pack . show <$> declarationLanguage declaration) else Nothing
    , symbolKind = when symbolFieldsShowKind (toCategoryName declaration)
    , symbolLine = when symbolFieldsShowLine (declarationText declaration)
    , symbolSpan = when symbolFieldsShowSpan (getField record)
    }
  _ -> Nothing

data File = File
  { filePath :: T.Text
  , fileLanguage :: Maybe T.Text
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
  toJSON Symbol{..} = objectWithoutNulls [ "symbol" .= symbolName
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
  { symbolFieldsShowName :: Bool
  , symbolFieldsShowPath :: Bool
  , symbolFieldsShowLang :: Bool
  , symbolFieldsShowKind :: Bool
  , symbolFieldsShowLine :: Bool
  , symbolFieldsShowSpan :: Bool
  }
  deriving (Eq, Show)

defaultSymbolFields :: SymbolFields
defaultSymbolFields = SymbolFields True False False True False True

defaultTagSymbolFields :: SymbolFields
defaultTagSymbolFields = SymbolFields True True True True True True
