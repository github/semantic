module Rendering.JSON
( renderJSONDiff
, renderJSONDiffs
, renderJSONTerm
, renderJSONTerms
) where

import Prologue
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Aeson as A
import Data.JSON.Fields
import Data.Blob
import qualified Data.Map as Map
import Data.Patch


-- | Render a diff to a value representing its JSON.
renderJSONDiff :: ToJSON a => BlobPair -> a -> [Value]
renderJSONDiff blobs diff = pure $
  toJSON (object [ "diff" .= diff, "stat" .= object (pathKey <> toJSONFields statPatch) ])
  where statPatch = these Delete Insert Replace (runJoin blobs)
        pathKey = [ "path" .= pathKeyForBlobPair blobs ]

renderJSONDiffs :: [Value] -> Map.Map Text Value
renderJSONDiffs = Map.singleton "diffs" . toJSON


-- | Render a term to a value representing its JSON.
renderJSONTerm :: ToJSON a => Blob -> a -> [Value]
renderJSONTerm blob content = pure $ toJSON (object ("programNode" .= content : toJSONFields blob))

renderJSONTerms :: [Value] -> Map.Map Text Value
renderJSONTerms = Map.singleton "trees" . toJSON
