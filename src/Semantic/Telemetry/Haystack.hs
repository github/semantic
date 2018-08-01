module Semantic.Telemetry.Haystack
  ( HaystackClient (..)
  , ErrorReport (..)
  , ErrorLogger
  , haystackClient
  , reportError
  ) where

import           Control.Exception
import           Crypto.Hash
import           Data.Aeson hiding (Error)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import           Prologue hiding (hash)
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
  , haystackClientAppName  :: String
  }                    -- ^ Standard HTTP client for Haystack
  | NullHaystackClient -- ^ Doesn't report needles, good for testing or when the 'HAYSTACK_URL' env var isn't set.

-- | Function to log if there are errors reporting to haystack.
type ErrorLogger = String -> [(String, String)] -> IO ()

-- Create a Haystack HTTP client.
haystackClient :: Maybe String -> ManagerSettings -> String -> IO HaystackClient
haystackClient maybeURL managerSettings appName
  | Just url <- maybeURL = do
      manager  <- newManager managerSettings
      request' <- parseRequest url
      let request = request'
            { method = "POST"
            , requestHeaders = ("Content-Type", "application/json; charset=utf-8") : requestHeaders request'
            }
      pure $ HaystackClient request manager appName
  | otherwise = pure NullHaystackClient

-- Report an error to Haystack over HTTP (blocking).
reportError :: ErrorLogger -> HaystackClient -> ErrorReport -> IO ()
reportError logger NullHaystackClient ErrorReport{..} = let msg = takeWhile (/= '\n') (displayException errorReportException) in logger msg errorReportContext
reportError logger HaystackClient{..} ErrorReport{..} = do
  let fullMsg = displayException errorReportException
  let summary = takeWhile (/= '\n') fullMsg
  logger summary errorReportContext
  let payload = object $
        [ "app"       .= haystackClientAppName
        , "message"   .= summary
        , "class"     .= summary
        , "backtrace" .= fullMsg
        , "rollup"    .= rollup fullMsg
        ] <> foldr (\(k, v) acc -> Text.pack k .= v : acc) [] errorReportContext
  let request = haystackClientRequest { requestBody = RequestBodyLBS (encode payload) }

  response <- tryIOError $ httpLbs request haystackClientManager
  case response of
    Left e -> logger ("Failed to report error to haystack: " <> displayException e) []
    Right response -> do
      let status = statusCode (responseStatus response)
      if status /= 201
        then logger ("Failed to report error to haystack, status=" <> show status <> ".") []
        else pure ()
  where
    rollup :: String -> Text
    rollup = Text.decodeUtf8 . digestToHexByteString . md5 . BC.pack

    md5 :: ByteString -> Digest MD5
    md5 = hash
