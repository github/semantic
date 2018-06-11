module Semantic.Haystack where

import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.Aeson
import qualified Data.Text.Encoding as Text
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import           Prologue hiding (hash)
import           Semantic.Queue
import           System.IO.Error

data ErrorReport
  = ErrorReport
  { errorReportMessage :: Text
  , errorReportContext :: [(Text, Text)]
  }

data HaystackClient
  = HaystackClient
  { haystackClientRequest  :: Request
  , haystackClientManager  :: Manager
  , haystackClientHostName :: String
  , haystackClientAppName  :: String
  }
  | NullHaystackClient -- ^ Doesn't report needles, good for testing or when the 'HAYSTACK_URL' env var isn't set.

-- Queue an error to be reported
queueErrorReport :: MonadIO io => AsyncQueue ErrorReport HaystackClient -> Text -> [(Text, Text)] -> io ()
queueErrorReport q@AsyncQueue{..} message = liftIO . queue q . ErrorReport message

-- Create a Haystack HTTP client.
haystackClient :: Maybe String -> ManagerSettings -> String -> String -> IO HaystackClient
haystackClient maybeURL managerSettings hostName appName
  | Just url <- maybeURL = do
      manager  <- newManager managerSettings
      request' <- parseRequest url
      let request = request'
            { method = "POST"
            , requestHeaders = [ ("Content-Type", "application/json; charset=utf-8") ]
            }
      pure $ HaystackClient request manager hostName appName
  | otherwise = pure NullHaystackClient

-- Report an error to Haystack over HTTP (blocking).
--
-- Returns Left error if reporting failed (or if using the NullHaystackClient)
-- or Right status code received from sending the report.
reportError :: MonadIO io => HaystackClient -> ErrorReport -> io (Either String Int)
reportError NullHaystackClient _               = pure (Left "Error not reported. NullHaystackClient configured.")
reportError HaystackClient{..} ErrorReport{..} = do
  let payload = object $
        [ "app"     .= haystackClientAppName
        , "host"    .= haystackClientHostName
        , "message" .= errorReportMessage
        , "rollup"  .= rollup errorReportMessage
        ] <> foldr (\(k, v) acc -> k .= v : acc) [] errorReportContext
  liftIO $ print payload
  let request = haystackClientRequest { requestBody = RequestBodyLBS (encode payload) }

  response <- liftIO . tryIOError $ httpLbs request haystackClientManager
  case response of
    Left e -> pure $ Left (displayException e)
    Right response -> do
      let status = statusCode (responseStatus response)
      liftIO $ print status
      liftIO $ print (responseBody response)
      pure $ Right status
  where
    rollup :: Text -> Text
    rollup = Text.decodeUtf8 . digestToHexByteString . md5 . Text.encodeUtf8

    md5 :: ByteString -> Digest MD5
    md5 = hash
