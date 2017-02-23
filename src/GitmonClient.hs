{-# LANGUAGE RecordWildCards, BangPatterns, DeriveGeneric #-}
module GitmonClient where

import Arguments
import qualified Data.Yaml as Y
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (toStrict)
import GHC.Generics
import Git.Libgit2
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Prelude
import Prologue hiding (toStrict)
import System.Clock


data ProcIO = ProcIO {
    read_bytes :: Integer
  , write_bytes :: Integer
} deriving (Show, Generic)

instance FromJSON ProcIO


data ProcessStats =
    ProcessBeforeStats { gitDir :: String
                       , program :: String
                       , realIP :: Maybe String
                       , repoID :: Maybe String
                       , repoName :: Maybe String
                       , userID :: Maybe String
                       , via :: String }
  | ProcessAfterStats { cpu :: Integer
                      , diskReadBytes :: Integer
                      , diskWriteBytes :: Integer
                      , resultCode :: Integer } deriving (Generic, Show)

instance ToJSON ProcessStats where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }


data GitmonCommand = Update
                   | Finish
                   | Schedule deriving (Show)

instance ToJSON GitmonCommand where
  toJSON command = String $ case command of
    Update -> "update"
    Finish -> "finish"
    Schedule -> "schedule"


data GitmonMsg = GitmonMsg { command :: GitmonCommand, stats :: ProcessStats } deriving (Show)

instance ToJSON GitmonMsg where
  toJSON GitmonMsg{..} = object [
    "command" .= command,
    "data" .= stats
    ]


clock :: Clock
clock = Realtime

processJSON :: GitmonCommand -> ProcessStats -> ByteString
processJSON command stats = toStrict . encode $ GitmonMsg command stats

reportGitmon :: String -> Arguments -> ReaderT LgRepo IO a -> ReaderT LgRepo IO a
reportGitmon program Arguments{..} gitCommand = do
  soc <- liftIO $ socket AF_UNIX Stream defaultProtocol
  safeIO $ connect soc (SockAddrUnix "/tmp/gitstats.sock")

  safeIO $ sendAll soc (processJSON Update ProcessBeforeStats { gitDir = gitDir, via = "semantic-diff", program = program, realIP = realIP, repoID = repoID, repoName = repoName, userID = userID })

  startTime <- liftIO $ getTime clock
  beforeProcIOContents <- liftIO (Y.decodeFileEither "/proc/self/io" :: IO (Either Y.ParseException (Maybe ProcIO)))

  !result <- gitCommand

  safeIO $ sendAll soc (processJSON Finish ProcessAfterStats { cpu = 100, diskReadBytes = 1000, diskWriteBytes = 1000, resultCode = 0 })
  endTime <- liftIO $ getTime clock
  afterProcIOContents <- liftIO (Y.decodeFileEither "/proc/self/io" :: IO (Either Y.ParseException (Maybe ProcIO)))

  let beforeDiskReadBytes = either (const 0) (maybe 0 read_bytes) beforeProcIOContents
  let afterDiskReadBytes = either (const 0) (maybe 0 read_bytes) afterProcIOContents
  let beforeDiskWriteBytes = either (const 0) (maybe 0 write_bytes) beforeProcIOContents
  let afterDiskWriteBytes = either (const 0) (maybe 0 write_bytes) afterProcIOContents

  safeIO $ sendAll soc (processJSON Finish ProcessAfterStats { cpu = toNanoSecs endTime - toNanoSecs startTime, diskReadBytes = afterDiskReadBytes - beforeDiskReadBytes, diskWriteBytes = afterDiskWriteBytes - beforeDiskWriteBytes, resultCode = 0 })

  safeIO $ close soc

  return result

  where safeIO :: MonadIO m => IO () -> m ()
        safeIO command = liftIO $ command `catch` noop

        noop :: IOException -> IO ()
        noop _ = return ()
