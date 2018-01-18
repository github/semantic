{-# LANGUAGE DataKinds, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Rendering.Tag
( renderToTags
, TagFields(..)
, defaultTagFields
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
import Rendering.TOC

-- | Render a 'Term' to a ctags like output (See 'Tag').
renderToTags :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => TagFields -> Blob -> Term f (Record fields) -> [Value]
renderToTags fields Blob{..} = fmap toJSON . termToC fields blobPath
  where
    termToC :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => TagFields -> FilePath -> Term f (Record fields) -> [Tag]
    termToC fields path = mapMaybe (tagSummary fields path "unchanged") . termTableOfContentsBy declaration

-- | Construct a 'Tag' from a node annotation and a change type label.
tagSummary :: (HasField fields (Maybe Declaration), HasField fields Span) => TagFields -> FilePath -> T.Text -> Record fields -> Maybe Tag
tagSummary TagFields{..} path _ record = case getDeclaration record of
  Just ErrorDeclaration{} -> Nothing
  Just declaration -> Just $ Tag
    { tagSymbol = when tagFieldsShowSymbol (declarationIdentifier declaration)
    , tagPath = when tagFieldsShowPath (T.pack path)
    , tagLanguage = if tagFieldsShowLanguage then (T.pack . show <$> declarationLanguage declaration) else Nothing
    , tagKind = when tagFieldsShowKind (toCategoryName declaration)
    , tagLine = when tagFieldsShowLine (declarationText declaration)
    , tagSpan = when tagFieldsShowSpan (getField record)
    }
  _ -> Nothing

data Tag = Tag
  { tagSymbol :: Maybe T.Text
  , tagPath :: Maybe T.Text
  , tagLanguage :: Maybe T.Text
  , tagKind :: Maybe T.Text
  , tagLine :: Maybe T.Text
  , tagSpan :: Maybe Span
  } deriving (Generic, Eq, Show)

instance ToJSON Tag where
  toJSON Tag{..} = objectWithoutNulls [ "symbol" .= tagSymbol
                          , "path" .= tagPath
                          , "language" .= tagLanguage
                          , "kind" .= tagKind
                          , "line" .= tagLine
                          , "span" .= tagSpan ]
    where objectWithoutNulls = object . filter (\(_, v) -> v /= Null)

when :: Bool -> a -> Maybe a
when True a = Just a
when False _ = Nothing

data TagFields = TagFields
  { tagFieldsShowSymbol :: Bool
  , tagFieldsShowPath :: Bool
  , tagFieldsShowLanguage :: Bool
  , tagFieldsShowKind :: Bool
  , tagFieldsShowLine :: Bool
  , tagFieldsShowSpan :: Bool
  }
  deriving (Eq, Show)

defaultTagFields :: TagFields
defaultTagFields = TagFields True True True True True True
