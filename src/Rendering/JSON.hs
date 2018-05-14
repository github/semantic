{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
module Rendering.JSON
( JSONOutput(..)
, toJSONOutput
, JSONTrees(..)
, renderJSONDiff
, renderJSONTerm
, renderJSONAST
, JSONAST(..)
, renderSymbolTerms
, SomeJSON(..)
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


newtype JSONTrees a = JSONTrees { unJSONTrees :: [a] }
  deriving (Eq, Monoid, Semigroup, Show)

instance ToJSON a => ToJSON (JSONTrees a) where
  toJSON (JSONTrees terms) = object ["trees" .= terms]
  toEncoding (JSONTrees terms) = pairs ("trees" .= terms)

instance ToJSON a => Output (JSONTrees a) where
  toOutput = (<> "\n") . fromEncoding . toEncoding


-- | Render a term to a value representing its JSON.
renderJSONTerm :: ToJSON a => Blob -> a -> JSONTrees SomeJSON
renderJSONTerm blob content = JSONTrees [ SomeJSON (JSONTerm blob content) ]

data JSONTerm a = JSONTerm { jsonTermBlob :: Blob, jsonTerm :: a }
  deriving (Eq, Show)

instance ToJSON a => ToJSON (JSONTerm a) where
  toJSON JSONTerm{..} = object ("ast" .= jsonTerm : toJSONFields jsonTermBlob)
  toEncoding JSONTerm{..} = pairs (fold ("ast" .= jsonTerm : toJSONFields jsonTermBlob))


renderJSONAST :: ToJSON a => Blob -> a -> JSONTrees SomeJSON
renderJSONAST blob content = JSONTrees [ SomeJSON (JSONAST blob content) ]

data JSONAST a = JSONAST { jsonASTBlob :: Blob, jsonAST :: a }
  deriving (Eq, Show)

instance ToJSON a => ToJSON (JSONAST a) where
  toJSON JSONAST{..} = object ("ast" .= jsonAST : toJSONFields jsonASTBlob)
  toEncoding JSONAST{..} = pairs (fold ("ast" .= jsonAST : toJSONFields jsonASTBlob))


-- | Render terms to final JSON structure.
renderSymbolTerms :: [Value] -> JSONOutput
renderSymbolTerms = toJSONOutput "files"


data SomeJSON where
  SomeJSON :: ToJSON a => a -> SomeJSON

instance ToJSON SomeJSON where
  toJSON (SomeJSON a) = toJSON a
  toEncoding (SomeJSON a) = toEncoding a
