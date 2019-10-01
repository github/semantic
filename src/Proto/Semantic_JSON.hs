{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Proto.Semantic_JSON where

import Control.Lens hiding ((.=))
import Data.Aeson as A
import Data.ProtoLens (defMessage)
import Proto.Semantic as P
import Proto.Semantic_Fields as P

instance FromJSON PingRequest where
  parseJSON = withObject "PingRequest" $ \obj -> do
    service <- obj .: "service"
    pure $ defMessage & P.service .~ service

instance ToJSON PingRequest where
  toJSON x = object [ "service" .= (x^.service) ]
  toEncoding x = pairs $ "service" .= (x^.service)

instance FromJSON PingResponse where
  parseJSON = A.withObject "PingResponse" $ \obj -> do
    status <- obj .: "status"
    hostname <- obj .: "hostname"
    timestamp <- obj .: "timestamp"
    sha <- obj .: "sha"
    pure $ defMessage
      & P.status .~ status
      & P.hostname .~ hostname
      & P.timestamp .~ timestamp
      & P.sha .~ sha

instance ToJSON PingResponse where
  toJSON x = object
    [ "status" .= (x^.status)
    , "hostname" .= (x^.hostname)
    , "timestamp" .= (x^.timestamp)
    , "sha" .= (x^.sha)
    ]
  toEncoding x = pairs $
       "status" .= (x^.status)
    <> "hostname" .= (x^.hostname)
    <> "timestamp" .= (x^.timestamp)
    <> "sha" .= (x^.sha)
