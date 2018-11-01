{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Rendering.Symbol
( renderToSymbols
, renderToSymbols'
, SymbolFields(..)
, defaultSymbolFields
, parseSymbolFields
) where

import Prologue hiding (when)
import Analysis.Declaration
import Data.Aeson
import Data.Blob
import Data.Language (ensureLanguage)
import Data.Location
import Data.List.Split (splitWhen)
import Data.Term
import qualified Data.Text as T
import Rendering.TOC
import Tags.Tagging
import Tags.Taggable


-- | Render a 'Term' to a list of symbols (See 'Symbol').
renderToSymbols :: (Foldable f, Functor f) => SymbolFields -> Blob -> Term f (Maybe Declaration) -> [Value]
renderToSymbols fields Blob{..} term = [toJSON (termToC fields blobPath term)]
  where
    termToC :: (Foldable f, Functor f) => SymbolFields -> FilePath -> Term f (Maybe Declaration) -> File
    termToC fields path = File (T.pack path) (T.pack (show blobLanguage)) . mapMaybe (symbolSummary fields path "unchanged") . termTableOfContentsBy declaration

renderToSymbols' :: (IsTaggable fs) => SymbolFields -> Blob -> Term (Sum fs) Location -> [Tag]
renderToSymbols' _ blob term = either mempty id (runTagging blob term)

-- | Construct a 'Symbol' from a node annotation and a change type label.
symbolSummary :: SymbolFields -> FilePath -> T.Text -> Declaration -> Maybe Symbol
symbolSummary SymbolFields{..} path _ record = case record of
  ErrorDeclaration{} -> Nothing
  declaration -> Just Symbol
    { symbolName = when symbolFieldsName (declarationIdentifier declaration)
    , symbolPath = when symbolFieldsPath (T.pack path)
    , symbolLang = join (when symbolFieldsLang (T.pack . show <$> ensureLanguage (declarationLanguage declaration)))
    , symbolKind = when symbolFieldsKind (toCategoryName declaration)
    , symbolLine = when symbolFieldsLine (declarationText declaration)
    , symbolSpan = when symbolFieldsSpan (declarationSpan declaration)
    }

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
