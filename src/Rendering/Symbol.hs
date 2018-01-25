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
import Control.Monad (join)
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Map as Map
import Rendering.TOC


-- | Render a 'Term' to a ctags like output (See 'Tag').
--
-- This format is going away. Prefer the new 'renderToSymbols' as it provides a
-- more compact data representation and custom field selection. This exists to
-- back support the staff shipped tag generation in github/github.
renderToTags :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => Blob -> Term f (Record fields) -> [Value]
renderToTags Blob{..} = fmap toJSON . termToC blobPath
  where
    termToC :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => FilePath -> Term f (Record fields) -> [Symbol]
    termToC path = mapMaybe (symbolSummary defaultTagSymbolFields path "unchanged") . termTableOfContentsBy declaration


-- | Render terms to final JSON structure.
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
  Just declaration -> Just Symbol
    { symbolName = when symbolFieldsName (declarationIdentifier declaration)
    , symbolPath = when symbolFieldsPath (T.pack path)
    , symbolLang = join (when symbolFieldsLang (T.pack . show <$> declarationLanguage declaration))
    , symbolKind = when symbolFieldsKind (toCategoryName declaration)
    , symbolLine = when symbolFieldsLine (declarationText declaration)
    , symbolSpan = when symbolFieldsSpan (getField record)
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
