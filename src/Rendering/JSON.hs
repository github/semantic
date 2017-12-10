module Rendering.JSON
( renderJSONDiff
, renderJSONTerm
) where

import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Aeson as A
import Data.Blob
import Data.Bifoldable (biList)
import Data.Language
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics

--
-- Diffs
--

-- | Render a diff to a string representing its JSON.
renderJSONDiff :: ToJSON a => BlobPair -> a -> Map.Map Text Value
renderJSONDiff blobs diff = Map.fromList
  [ ("diff", toJSON diff)
  , ("oids", toJSON (decodeUtf8 . blobOid <$> biList blobs))
  , ("paths", toJSON (blobPath <$> biList blobs))
  ]

data File a = File { filePath :: FilePath, fileLanguage :: Maybe Language, fileContent :: a }
  deriving (Generic, Show)

instance ToJSON a => ToJSON (File a) where
  toJSON File{..} = object [ "filePath" .= filePath, "language" .= fileLanguage, "programNode" .= fileContent ]

renderJSONTerm :: ToJSON a => Blob -> a -> [Value]
renderJSONTerm Blob{..} = pure . toJSON . File blobPath blobLanguage
