{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rendering.JSON
( JSONOutput(..)
, toJSONOutput
, JSONTerms(..)
, renderJSONDiff
, renderJSONTerm
, renderJSONAST
, JSONAST(..)
, renderSymbolTerms
) where

import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Aeson as A
import Data.JSON.Fields
import Data.Blob
import qualified Data.Map.Monoidal as Monoidal
import Data.Output
import Data.Patch
import Prologue

newtype JSONOutput = JSONOutput { unJSONOutput :: Monoidal.Map Text [Value] }
  deriving (Eq, Monoid, Semigroup, Show, ToJSON)

toJSONOutput :: Text -> [Value] -> JSONOutput
toJSONOutput key = JSONOutput . Monoidal.singleton key

instance Output JSONOutput where
  toOutput = (<> "\n") . fromEncoding . toEncoding


-- | Render a diff to a value representing its JSON.
renderJSONDiff :: ToJSON a => BlobPair -> a -> JSONOutput
renderJSONDiff blobs diff = renderJSONDiffs
  [ toJSON (object [ "diff" .= diff, "stat" .= object (pathKey <> toJSONFields statPatch) ]) ]
  where statPatch = these Delete Insert Replace (runJoin blobs)
        pathKey = [ "path" .= pathKeyForBlobPair blobs ]

renderJSONDiffs :: [Value] -> JSONOutput
renderJSONDiffs = toJSONOutput "diffs"


newtype JSONTerms = JSONTerms { unJSONTerms :: [Value] }
  deriving (Eq, Monoid, Semigroup, Show, ToJSON)

instance Output JSONTerms where
  toOutput = (<> "\n") . fromEncoding . toEncoding . Monoidal.singleton ("trees" :: Text) . unJSONTerms

-- | Render a term to a value representing its JSON.
renderJSONTerm :: ToJSON a => Blob -> a -> JSONTerms
renderJSONTerm blob content = JSONTerms [ toJSON (object ("programNode" .= content : toJSONFields blob)) ]

renderJSONAST :: ToJSON a => Blob -> a -> JSONTerms
renderJSONAST blob content = JSONTerms [ toJSON (JSONAST blob content) ]

data JSONAST a = JSONAST { jsonASTBlob :: Blob, jsonAST :: a }
  deriving (Eq, Show)

instance ToJSON a => ToJSON (JSONAST a) where
  toJSON JSONAST{..} = object ("ast" .= jsonAST : toJSONFields jsonASTBlob)
  toEncoding JSONAST{..} = pairs (fold ("ast" .= jsonAST : toJSONFields jsonASTBlob))


-- | Render terms to final JSON structure.
renderSymbolTerms :: [Value] -> JSONOutput
renderSymbolTerms = toJSONOutput "files"
