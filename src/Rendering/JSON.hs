{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving, KindSignatures, ScopedTypeVariables #-}
module Rendering.JSON
( JSON(..)
, renderJSONDiff
, renderJSONTerm
, renderJSONAST
, renderSymbolTerms
, renderJSONError
, SomeJSON(..)
) where

import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Aeson as A
import Data.JSON.Fields
import Data.Blob
import Data.Patch
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

newtype JSONStat = JSONStat { jsonStatBlobs :: BlobPair }
  deriving (Eq, Show)

instance ToJSON JSONStat where
  toJSON JSONStat{..} = object ("path" .= pathKeyForBlobPair jsonStatBlobs : toJSONFields (these Delete Insert Replace (runJoin jsonStatBlobs)))
  toEncoding JSONStat{..} = pairs (fold ("path" .= pathKeyForBlobPair jsonStatBlobs : toJSONFields (these Delete Insert Replace (runJoin jsonStatBlobs))))

newtype JSONError = JSONError { jsonError :: String }
  deriving (Eq, Show)

-- | Render a term to a value representing its JSON.
renderJSONTerm :: ToJSON a => Blob -> a -> JSON "trees" SomeJSON
renderJSONTerm blob content = JSON [ SomeJSON (JSONTerm blob content) ]

data JSONTerm a = JSONTerm { jsonTermBlob :: Blob, jsonTerm :: a }
  deriving (Eq, Show)

instance ToJSON a => ToJSON (JSONTerm a) where
  toJSON JSONTerm{..} = object ("tree" .= jsonTerm : toJSONFields jsonTermBlob)
  toEncoding JSONTerm{..} = pairs (fold ("tree" .= jsonTerm : toJSONFields jsonTermBlob))


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

renderJSONError :: Blob -> String -> JSON "trees" SomeJSON
renderJSONError Blob{..} e = JSON [ SomeJSON (object [ "error" .= err ]) ]
  where err = object [ "message" .= e
                     , "path" .= blobPath
                     , "language" .= blobLanguage ]

data SomeJSON where
  SomeJSON :: ToJSON a => a -> SomeJSON

instance ToJSON SomeJSON where
  toJSON (SomeJSON a) = toJSON a
  toEncoding (SomeJSON a) = toEncoding a
