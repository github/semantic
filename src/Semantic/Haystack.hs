module Semantic.Haystack where

import           Control.Exception
import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.Aeson hiding (Error)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import           Prologue hiding (hash)
import           Semantic.Log
import           Semantic.Queue
import           System.IO.Error

data ErrorReport
  = ErrorReport
  { errorReportException :: SomeException
  , errorReportContext   :: [(String, String)]
  } deriving (Show)

data HaystackClient
  = HaystackClient
  { haystackClientRequest  :: Request
  , haystackClientManager  :: Manager
  , haystackClientHostName :: String
  , haystackClientAppName  :: String
  }
  | NullHaystackClient -- ^ Doesn't report needles, good for testing or when the 'HAYSTACK_URL' env var isn't set.

-- Queue an error to be reported to haystack.
queueErrorReport :: MonadIO io => AsyncQueue ErrorReport HaystackClient -> SomeException -> [(String, String)] -> io ()
queueErrorReport q@AsyncQueue{..} message = liftIO . queue q . ErrorReport message

-- Create a Haystack HTTP client.
haystackClient :: Maybe String -> ManagerSettings -> String -> String -> IO HaystackClient
haystackClient maybeURL managerSettings hostName appName
  | Just url <- maybeURL = do
      manager  <- newManager managerSettings
      request' <- parseRequest url
      let request = request'
            { method = "POST"
            , requestHeaders = ("Content-Type", "application/json; charset=utf-8") : requestHeaders request'
            }
      pure $ HaystackClient request manager hostName appName
  | otherwise = pure NullHaystackClient

-- Report an error to Haystack over HTTP (blocking).
reportError :: MonadIO io => String -> LogQueue -> HaystackClient -> ErrorReport -> io ()
reportError sha logger NullHaystackClient ErrorReport{..} =
  let msg = takeWhile (/= '\n') (displayException errorReportException) in
  queueLogMessage logger Error msg errorReportContext
reportError sha logger HaystackClient{..} ErrorReport{..} = do
  let fullMsg = displayException errorReportException
  let summary = takeWhile (/= '\n') fullMsg
  queueLogMessage logger Error summary errorReportContext
  let payload = object $
        [ "app"       .= haystackClientAppName
        , "host"      .= haystackClientHostName
        , "sha"       .= sha
        , "message"   .= summary
        , "class"     .= summary
        , "backtrace" .= fullMsg
        , "rollup"    .= rollup fullMsg
        ] <> foldr (\(k, v) acc -> Text.pack k .= v : acc) [] errorReportContext
  let request = haystackClientRequest { requestBody = RequestBodyLBS (encode payload) }

  response <- liftIO . tryIOError $ httpLbs request haystackClientManager
  case response of
    Left e -> queueLogMessage logger Error ("Failed to report error to haystack: " <> displayException e) []
    Right response -> do
      let status = statusCode (responseStatus response)
      if status /= 201
        then queueLogMessage logger Error ("Failed to report error to haystack, status=" <> show status <> ".") []
        else pure ()
  where
    rollup :: String -> Text
    rollup = Text.decodeUtf8 . digestToHexByteString . md5 . BC.pack

    md5 :: ByteString -> Digest MD5
    md5 = hash
