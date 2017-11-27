{-# LANGUAGE DataKinds, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Rendering.Tag
( renderToTags
) where

import Data.Aeson
import Data.Blob
import Data.Maybe (mapMaybe)
import Data.Record
import Data.Term
import GHC.Generics
import Info
import qualified Data.Text as T
import Rendering.TOC

-- | Render a 'Term' to a ctags like output (See 'Tag').
renderToTags :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => Blob -> Term f (Record fields) -> [Value]
renderToTags Blob{..} = fmap toJSON . termToC blobPath
  where
    termToC :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => FilePath -> Term f (Record fields) -> [Tag]
    termToC path = mapMaybe (tagSummary path "unchanged") . termTableOfContentsBy declaration

-- | Construct a 'Tag' from a node annotation and a change type label.
tagSummary :: (HasField fields (Maybe Declaration), HasField fields Span) => FilePath -> T.Text -> Record fields -> Maybe Tag
tagSummary path _ record = case getDeclaration record of
  Just ErrorDeclaration{} -> Nothing
  Just declaration -> Just $ Tag (declarationIdentifier declaration) (T.pack path) (T.pack . show <$> declarationLanguage declaration) (toCategoryName declaration) (declarationText declaration) (sourceSpan record)
  _ -> Nothing

data Tag
  = Tag { tagSymbol :: T.Text
        , tagPath :: T.Text
        , tagLanguage :: Maybe T.Text
        , tagKind :: T.Text
        , tagLine :: T.Text
        , tagSpan :: Span
      }
  deriving (Generic, Eq, Show)

instance ToJSON Tag where
  toJSON Tag{..} = object [ "symbol" .= tagSymbol
                          , "path" .= tagPath
                          , "language" .= tagLanguage
                          , "kind" .= tagKind
                          , "line" .= tagLine
                          , "span" .= tagSpan ]
