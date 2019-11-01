{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving, KindSignatures, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TypeApplications #-}
module Rendering.JSON
( JSON(..)
, renderJSONDiff
, renderJSONAdjDiff
, renderJSONTerm
, renderJSONAdjTerm
, renderJSONAST
, renderSymbolTerms
, renderJSONError
, renderJSONSymbolError
, renderJSONDiffError
, SomeJSON(..)
) where

import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Aeson as A
import Data.Blob
import Data.JSON.Fields
import Data.Text (pack)
import GHC.TypeLits
import Prologue

newtype JSON (key :: Symbol) a = JSON { unJSON :: [a] }
  deriving (Eq, Monoid, Semigroup, Show)

instance (KnownSymbol key, ToJSON a) => ToJSON (JSON key a) where
  toJSON (JSON as) = object [ pack (symbolVal @key undefined) .= as ]
  toEncoding (JSON as) = pairs (pack (symbolVal @key undefined) .= as)


-- | Render a diff to a value representing its JSON.
renderJSONDiff :: ToJSON a => BlobPair -> a -> JSON "diffs" SomeJSON
renderJSONDiff blobs diff = JSON [ SomeJSON (JSONDiff (JSONStat blobs) diff) ]

data JSONDiff a = JSONDiff { jsonDiffStat :: JSONStat, jsonDiff :: a }
  deriving (Eq, Show)

instance ToJSON a => ToJSON (JSONDiff a) where
  toJSON JSONDiff{..} = object [ "diff" .= jsonDiff, "stat" .= jsonDiffStat ]
  toEncoding JSONDiff{..} = pairs ("diff" .= jsonDiff <> "stat" .= jsonDiffStat)

-- | Render a diff to a value representing its JSON.
renderJSONAdjDiff :: ToJSON a => BlobPair -> a -> JSON "diffs" SomeJSON
renderJSONAdjDiff blobs diff = JSON [ SomeJSON (JSONAdjDiff (JSONStat blobs) diff) ]

data JSONAdjDiff a = JSONAdjDiff { jsonAdjDiffStat :: JSONStat, jsonAdjDiff :: a }
  deriving (Eq, Show)

instance ToJSON a => ToJSON (JSONAdjDiff a) where
  toJSON JSONAdjDiff{..} = object [ "graph" .= jsonAdjDiff, "stat" .= jsonAdjDiffStat ]
  toEncoding JSONAdjDiff{..} = pairs ("graph" .= jsonAdjDiff <> "stat" .= jsonAdjDiffStat)

newtype JSONStat = JSONStat { jsonStatBlobs :: BlobPair }
  deriving (Eq, Show)

instance ToJSON JSONStat where
  toJSON JSONStat{..} = object ("path" .= pathKeyForBlobPair jsonStatBlobs : toJSONFields jsonStatBlobs)
  toEncoding JSONStat{..} = pairs (fold ("path" .= pathKeyForBlobPair jsonStatBlobs : toJSONFields jsonStatBlobs))

-- | Render a term to a value representing its JSON.
renderJSONTerm :: ToJSON a => Blob -> a -> JSON "trees" SomeJSON
renderJSONTerm blob content = JSON [ SomeJSON (JSONTerm blob content) ]

data JSONTerm a = JSONTerm { jsonTermBlob :: Blob, jsonTerm :: a }
  deriving (Eq, Show)

instance ToJSON a => ToJSON (JSONTerm a) where
  toJSON JSONTerm{..} = object ("tree" .= jsonTerm : toJSONFields jsonTermBlob)
  toEncoding JSONTerm{..} = pairs (fold ("tree" .= jsonTerm : toJSONFields jsonTermBlob))

renderJSONAdjTerm :: ToJSON a => Blob -> a -> JSON "trees" SomeJSON
renderJSONAdjTerm blob content = JSON [ SomeJSON (JSONAdjTerm blob content) ]

data JSONAdjTerm a = JSONAdjTerm { jsonAdjTermBlob :: Blob, jsonAdjTerm :: a }
  deriving (Eq, Show)

instance ToJSON a => ToJSON (JSONAdjTerm a) where
  toJSON JSONAdjTerm{..} = object ("graph" .= jsonAdjTerm : toJSONFields jsonAdjTermBlob)
  toEncoding JSONAdjTerm{..} = pairs (fold ("graph" .= jsonAdjTerm : toJSONFields jsonAdjTermBlob))

renderJSONAST :: ToJSON a => Blob -> a -> JSON "trees" SomeJSON
renderJSONAST blob content = JSON [ SomeJSON (JSONAST blob content) ]

data JSONAST a = JSONAST { jsonASTBlob :: Blob, jsonAST :: a }
  deriving (Eq, Show)

instance ToJSON a => ToJSON (JSONAST a) where
  toJSON JSONAST{..} = object ("ast" .= jsonAST : toJSONFields jsonASTBlob)
  toEncoding JSONAST{..} = pairs (fold ("ast" .= jsonAST : toJSONFields jsonASTBlob))


-- | Render terms to final JSON structure.
renderSymbolTerms :: ToJSON a => [a] -> JSON "files" SomeJSON
renderSymbolTerms = JSON . map SomeJSON

-- | Render an error for symbols.
renderJSONSymbolError :: Blob -> String -> JSON "files" SomeJSON
renderJSONSymbolError blob e = JSON [ renderError blob e ]

-- | Render an error for terms.
renderJSONError :: Blob -> String -> JSON "trees" SomeJSON
renderJSONError blob e = JSON [ renderError blob e ]

-- | Render an error for a particular blob.
renderError :: ToJSON a => Blob -> a -> SomeJSON
renderError b e = SomeJSON $ object
  [ "error"    .= e
  , "path"     .= blobPath b
  , "language" .= blobLanguage b
  ]

-- | Render an error for diffs.
renderJSONDiffError :: BlobPair -> String -> JSON "diffs" SomeJSON
renderJSONDiffError pair e = JSON [ SomeJSON (object [ "error" .= err ]) ]
  where err = object ["message" .= e, "stat" .= toJSON (JSONStat pair)]

data SomeJSON where
  SomeJSON :: ToJSON a => a -> SomeJSON

instance ToJSON SomeJSON where
  toJSON (SomeJSON a) = toJSON a
  toEncoding (SomeJSON a) = toEncoding a
