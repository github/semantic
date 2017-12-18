module Rendering.JSON
( renderJSONDiff
, renderJSONDiffs
, renderJSONTerm
, renderJSONTerms
, renderJSONMetadata
) where

import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Aeson as A
import Data.JSON.Fields
import Data.Blob
import Data.Bifunctor.Join
import qualified Data.Map as Map
import Data.Text (Text)
import Semantic.Log
import Data.Patch
import Data.Monoid
import Data.These


-- | Render a diff to a value representing its JSON.
renderJSONDiff :: ToJSON a => BlobPair -> a -> [Value]
renderJSONDiff blobs diff = pure $
  toJSON (object [ "diff" .= diff, "stat" .= object (pathKey <> toJSONFields statPatch) ])
  where statPatch = these Delete Insert Replace (runJoin blobs)
        pathKey = [ "path" .= pathKeyForBlobPair blobs ]

renderJSONDiffs :: Options -> [Value] -> Map.Map Text Value
renderJSONDiffs options = Map.union (renderJSONMetadata options) . Map.singleton "diffs" . toJSON


-- | Render a term to a value representing its JSON.
renderJSONTerm :: ToJSON a => Blob -> a -> [Value]
renderJSONTerm blob content = pure $ toJSON (object ("programNode" .= content : toJSONFields blob))

renderJSONTerms :: Options -> [Value] -> Map.Map Text Value
renderJSONTerms options = Map.union (renderJSONMetadata options) . Map.singleton "trees" . toJSON

-- | Render program options/metadata to a map representing its JSON.
renderJSONMetadata :: Options -> Map.Map Text Value
renderJSONMetadata Options{..} = Map.singleton "_metadata" $
  toJSON (object [ "version" .= optionsLibraryVersion
                 , "sha" .= optionsGitHash
                 , "requestId" .= optionsRequestID
                 ])
