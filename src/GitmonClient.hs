-- | We use BangPatterns to force evaluation of git operations to preserve accuracy in measuring system stats (particularly disk read bytes)
{-# LANGUAGE RecordWildCards, DeriveGeneric, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module GitmonClient where

import Control.Exception (throw)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (toStrict)
import Data.Text (pack, unpack, toLower, isInfixOf)
import qualified Data.Yaml as Y
import GHC.Generics
import Git.Libgit2
import Network.Socket hiding (recv)
import Network.Socket.ByteString (sendAll, recv)
import Prelude
import Prologue hiding (toStrict)
import System.Clock
import System.Directory (getCurrentDirectory)
import System.Environment
import System.Timeout
import Text.Regex

newtype GitmonException = GitmonException String deriving (Show, Typeable)

instance Exception GitmonException


data ProcIO = ProcIO { read_bytes :: Integer
                     , write_bytes :: Integer } deriving (Show, Generic)

instance FromJSON ProcIO


data ProcessData = ProcessUpdateData { gitDir :: String
                                     , program :: String
                                     , realIP :: Maybe String
                                     , repoName :: Maybe String
                                     , repoID :: Maybe Int
                                     , userID :: Maybe Int
                                     , via :: String }
                 | ProcessScheduleData
                 | ProcessFinishData { cpu :: Integer
                                     , diskReadBytes :: Integer
                                     , diskWriteBytes :: Integer
                                     , resultCode :: Integer } deriving (Generic, Show)

instance ToJSON ProcessData where
  toJSON ProcessUpdateData{..} = object [ "git_dir" .= gitDir, "program" .= program, "repo_name" .= repoName, "real_ip" .= realIP, "repo_id" .= repoID, "user_id" .= userID, "via" .= via ]
  toJSON ProcessScheduleData = object []
  toJSON ProcessFinishData{..} = object [ "cpu" .= cpu, "disk_read_bytes" .= diskReadBytes, "disk_write_bytes" .= diskWriteBytes, "result_code" .= resultCode ]


data GitmonCommand = Update
                   | Finish
                   | Schedule deriving (Generic, Show)

instance ToJSON GitmonCommand where
  toJSON = genericToJSON defaultOptions { constructorTagModifier = unpack . toLower . pack }


data GitmonMsg = GitmonMsg { command :: GitmonCommand
                           , processData :: ProcessData } deriving (Show)

instance ToJSON GitmonMsg where
  toJSON GitmonMsg{..} = case command of
    Update -> object ["command" .= ("update" :: String), "data" .= processData]
    Finish -> object ["command" .= ("finish" :: String), "data" .= processData]
    Schedule -> object ["command" .= ("schedule" :: String)]


type ProcInfo = Either Y.ParseException (Maybe ProcIO)

newtype SocketFactory = SocketFactory { withSocket :: forall a. (Socket -> IO a) -> IO a }

reportGitmon :: String -> ReaderT LgRepo IO a -> ReaderT LgRepo IO a
reportGitmon = reportGitmon' SocketFactory { withSocket = withGitmonSocket }

reportGitmon' :: SocketFactory -> String -> ReaderT LgRepo IO a -> ReaderT LgRepo IO a
reportGitmon' SocketFactory{..} program gitCommand =
  join . liftIO . withSocket $ \socket' -> do
    (gitDir, realIP, repoName, repoID, userID) <- loadEnvVars
    safeGitmonIO . sendAll socket' $ processJSON Update (ProcessUpdateData gitDir program realIP repoName repoID userID "semantic-diff")
    safeGitmonIO . sendAll socket' $ processJSON Schedule ProcessScheduleData
    gitmonStatus <- safeGitmonIO $ recv socket' 1024

    (startTime, beforeProcIOContents) <- collectStats
    let result = withGitmonStatus gitmonStatus gitCommand
    (afterTime, afterProcIOContents) <- collectStats

    let (cpuTime, diskReadBytes, diskWriteBytes, resultCode) = procStats startTime afterTime beforeProcIOContents afterProcIOContents
    safeGitmonIO . sendAll socket' $ processJSON Finish (ProcessFinishData cpuTime diskReadBytes diskWriteBytes resultCode)
    pure result

  where
    withGitmonStatus :: Maybe ByteString -> ReaderT LgRepo IO a -> ReaderT LgRepo IO a
    withGitmonStatus maybeGitmonStatus gitCommand = case maybeGitmonStatus of
      Just gitmonStatus | "fail" `isInfixOf` decodeUtf8 gitmonStatus -> throwGitmonException gitmonStatus
      _ -> gitCommand

    throwGitmonException :: ByteString -> e
    throwGitmonException command = throw . GitmonException . unpack $ "Received: '" <> decodeUtf8 command <> "' from Gitmon"

    collectStats :: IO (TimeSpec, ProcInfo)
    collectStats = do
      time <- getTime clock
      procIOContents <- Y.decodeFileEither procFileAddr :: IO ProcInfo
      pure (time, procIOContents)

    procStats :: TimeSpec -> TimeSpec -> ProcInfo -> ProcInfo -> ( Integer, Integer, Integer, Integer )
    procStats beforeTime afterTime beforeProcIOContents afterProcIOContents = ( cpuTime, diskReadBytes, diskWriteBytes, resultCode )
      where
        -- | toNanoSecs converts TimeSpec to Integer, and we further convert this value to milliseconds (expected by Gitmon).
        cpuTime = div (1 * 1000 * 1000) . toNanoSecs $ afterTime - beforeTime
        beforeDiskReadBytes = either (const 0) (maybe 0 read_bytes) beforeProcIOContents
        afterDiskReadBytes = either (const 0) (maybe 0 read_bytes) afterProcIOContents
        beforeDiskWriteBytes = either (const 0) (maybe 0 write_bytes) beforeProcIOContents
        afterDiskWriteBytes = either (const 0) (maybe 0 write_bytes) afterProcIOContents
        diskReadBytes = afterDiskReadBytes - beforeDiskReadBytes
        diskWriteBytes = afterDiskWriteBytes - beforeDiskWriteBytes
        resultCode = 0

    loadEnvVars :: IO (String, Maybe String, Maybe String, Maybe Int, Maybe Int)
    loadEnvVars = do
      pwd <- safeGetCurrentDirectory
      gitDir <- fromMaybe pwd <$> lookupEnv "GIT_DIR"
      realIP <- lookupEnv "GIT_SOCKSTAT_VAR_real_ip"
      repoName <- lookupEnv "GIT_SOCKSTAT_VAR_repo_name"
      repoID <- lookupEnv "GIT_SOCKSTAT_VAR_repo_id"
      userID <- lookupEnv "GIT_SOCKSTAT_VAR_user_id"
      pure (gitDir, realIP, repoName, readIntFromEnv repoID, readIntFromEnv userID)
      where
        safeGetCurrentDirectory :: IO String
        safeGetCurrentDirectory = getCurrentDirectory `catch` handleIOException

        handleIOException :: IOException -> IO String
        handleIOException _ = pure ""

        readIntFromEnv :: Maybe String -> Maybe Int
        readIntFromEnv Nothing = Nothing
        readIntFromEnv (Just s) = readInt $ matchRegex regex s
          where
            -- | Expected format for userID and repoID is: "uint:123",
            -- | where "uint:" indicates an unsigned integer followed by an integer value.
            regex :: Regex
            regex = mkRegexWithOpts "^uint:([0-9]+)$" False True

            readInt :: Maybe [String] -> Maybe Int
            readInt (Just [s]) = Just (read s :: Int)
            readInt _ = Nothing

withGitmonSocket :: (Socket -> IO c) -> IO c
withGitmonSocket = bracket connectSocket close
  where
    connectSocket = do
      s <- socket AF_UNIX Stream defaultProtocol
      safeGitmonIO $ connect s (SockAddrUnix gitmonSocketAddr)
      pure s

-- Timeout in nanoseconds to wait before giving up on Gitmon response to schedule.
gitmonTimeout :: Int
gitmonTimeout = 1 * 1000 * 1000

gitmonSocketAddr :: String
gitmonSocketAddr = "/tmp/gitstats.sock"

safeGitmonIO :: MonadIO m => IO a -> m (Maybe a)
safeGitmonIO command = liftIO $ timeout gitmonTimeout command `catch` noop

noop :: IOException -> IO (Maybe a)
noop _ = pure Nothing

procFileAddr :: String
procFileAddr = "/proc/self/io"

clock :: Clock
clock = Realtime

processJSON :: GitmonCommand -> ProcessData -> ByteString
processJSON command processData = (toStrict . encode $ GitmonMsg command processData)

