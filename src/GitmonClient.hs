{-# LANGUAGE RecordWildCards, BangPatterns #-}
module GitmonClient where

import Prologue hiding (toStrict)
import Prelude
import Data.Aeson
import Data.Aeson.Types
import Git.Libgit2

import Network.Socket
import Network.Socket.ByteString (sendAll)

import Data.ByteString.Lazy (toStrict)

data Stats =
    StartStats { repoName :: String
               , via :: String
               , gitDir :: String
               , program :: String
               , realIP :: Maybe String
               , repoID :: Maybe String
               , userID :: Maybe String }
  | FinishStats { cpu :: Integer
                , diskReadBytes :: Integer
                , diskWriteBytes :: Integer
                , resultCode :: Integer } deriving (Generic, Show)

instance ToJSON Stats where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data GitmonCommand = Update
                   | Finish
                   | Schedule deriving (Show)

instance ToJSON GitmonCommand where
  toJSON command = String $ case command of
    Update -> "update"
    Finish -> "finish"
    Schedule -> "schedule"

data GitmonMsg = GitmonMsg { command :: GitmonCommand, stats :: Stats } deriving (Show)

instance ToJSON GitmonMsg where
  toJSON GitmonMsg{..} = object [
    "command" .= command,
    "data" .= stats
    ]

reportGitmon :: ReaderT LgRepo IO a -> ReaderT LgRepo IO a
reportGitmon command = do
  soc <- liftIO $ socket AF_UNIX Stream defaultProtocol
  safeIO $ connect soc (SockAddrUnix "/tmp/gitstats.sock")

  let startStats = StartStats { repoName = "test-js", via = "gitrpc", gitDir = "/Users/vera/github/test-js", program = "semantic-diff", realIP = Nothing, repoID = Nothing, userID = Nothing }
  let startStatsJSON = toStrict . encode $ GitmonMsg Update startStats
  safeIO $ sendAll soc startStatsJSON

  result <- command

  let finishStats = FinishStats { cpu = 100, diskReadBytes = 1000, diskWriteBytes = 1000, resultCode = 0 }
  let finishStatsJSON = toStrict . encode $ GitmonMsg Finish finishStats
  !result <- command

  safeIO $ sendAll soc finishStatsJSON `catch` noop

  safeIO $ close soc `catch` noop

  return result

  where safeIO :: MonadIO m => IO () -> m ()
        safeIO command = liftIO $ command `catch` noop
        
        noop :: IOException -> IO ()
        noop _ = return ()
