module Semantic.Haystack where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Prologue hiding (hash)
import Semantic.Queue
import System.IO.Error
import qualified Data.Text.Encoding as Text
import Crypto.Hash

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

-- Report an error to Haystack over HTTP.
reportError :: MonadIO io => HaystackClient -> ErrorReport -> io (Maybe Int)
reportError NullHaystackClient _               = pure (Just 0)
reportError HaystackClient{..} ErrorReport{..} = do
  let payload = object $
        [ "app"     .= haystackClientAppName
        , "host"    .= haystackClientHostName
        , "message" .= errorReportMessage
        , "rollup"  .= rollup errorReportMessage
        ] <> foldr (\(k, v) acc -> k .= v : acc) [] errorReportContext
  let request = haystackClientRequest { requestBody = RequestBodyLBS (encode payload) }

  response <- liftIO . tryIOError $ httpLbs request haystackClientManager
  case response of
    Left _ -> pure Nothing
    Right response -> do
      let status = statusCode (responseStatus response)
      liftIO $ print status
      liftIO $ print (responseBody response)
      pure (Just status)

rollup :: Text -> Text
rollup = Text.decodeUtf8 . digestToHexByteString . md5 . Text.encodeUtf8
  where
    md5 :: ByteString -> Digest MD5
    md5 = hash
