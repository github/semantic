{-# LANGUAGE RecordWildCards, BangPatterns #-}
module GitmonClient where

import Prologue hiding (toStrict)
import Prelude
import Data.Aeson
import Data.Aeson.Types
import Git.Libgit2
import Arguments

import Network.Socket
import Network.Socket.ByteString (sendAll)

import Data.ByteString.Lazy (toStrict)

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

processJSON :: GitmonCommand -> ProcessStats -> ByteString
processJSON command stats = toStrict . encode $ GitmonMsg command stats

reportGitmon :: String -> Arguments -> ReaderT LgRepo IO a -> ReaderT LgRepo IO a
reportGitmon program Arguments{..} gitCommand = do
  soc <- liftIO $ socket AF_UNIX Stream defaultProtocol
  safeIO $ connect soc (SockAddrUnix "/tmp/gitstats.sock")

  safeIO $ sendAll soc (processJSON Update ProcessBeforeStats { gitDir = gitDir, via = "semantic-diff", program = program, realIP = realIP, repoID = repoID, repoName = repoName, userID = userID })

  !result <- gitCommand

  safeIO $ sendAll soc (processJSON Finish ProcessAfterStats { cpu = 100, diskReadBytes = 1000, diskWriteBytes = 1000, resultCode = 0 })

  safeIO $ close soc

  return result

  where safeIO :: MonadIO m => IO () -> m ()
        safeIO command = liftIO $ command `catch` noop

        noop :: IOException -> IO ()
        noop _ = return ()
