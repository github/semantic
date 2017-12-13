module Rendering.JSON
( renderJSONDiff
, renderJSONTerm
, renderJSONMetadata
) where

import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Aeson as A
import Data.Blob
import Data.Bifunctor
import Data.Bifoldable
import Data.Bifunctor.Join
import qualified Data.Map as Map
import Data.Text (Text)
import Semantic.Log

-- | Render a diff to a Map representing its JSON.
renderJSONDiff :: ToJSON a => BlobPair -> a -> Map.Map Text Value
renderJSONDiff blobs diff = Map.fromList $ ("diff", toJSON diff) : renderJSONBlobPair
  where
    renderJSONBlobPair = biList (bimap (render "before") (render "after") (runJoin blobs))
    render key Blob{..} = (key, toJSON (object [ "path" .= blobPath, "language" .= blobLanguage ]))

-- | Render a term to a Map representing its JSON.
renderJSONTerm :: ToJSON a => Blob -> a -> Map.Map Text Value
renderJSONTerm Blob{..} content = Map.singleton "trees" $
  toJSON (object [ "path" .= blobPath
                 , "language" .= blobLanguage
                 , "programNode" .= content
                 ])

renderJSONMetadata :: Options -> Map.Map Text Value
renderJSONMetadata Options{..} = Map.singleton "_metadata" $
  toJSON (object [ "version" .= optionsLibraryVersion
                 , "sha" .= optionsGitHash
                 , "requestId" .= optionsRequestID
                 ])
