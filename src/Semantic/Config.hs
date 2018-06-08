module Semantic.Config where

import Network.BSD
import Network.HTTP.Client.TLS
import Network.URI
import Prologue
import Semantic.Haystack
import Semantic.Log
import Semantic.Stat
import System.Environment
import System.IO (stderr)
import System.Posix.Process
import System.Posix.Types

data Config
  = Config
  { configAppName     :: String
  , configHostName    :: String    -- ^ HostName
  , configProcessID   :: ProcessID -- ^ ProcessID
  , configHaystackURL :: Maybe String
  , configStatsAddr   :: Addr
  , configLogOptions  :: Options
  }

data Addr = Addr { addrHost :: String, addrPort :: String }

defaultConfig :: IO Config
defaultConfig = do
  pid <- getProcessID
  hostName <- getHostName
  haystackURL <- lookupEnv "HAYSTACK_URL"
  statsAddr <- lookupStatsAddr
  logOptions <- configureOptionsForHandle stderr defaultOptions
  pure Config
    { configAppName = "semantic"
    , configHostName = hostName
    , configProcessID = pid
    , configHaystackURL = haystackURL
    , configStatsAddr = statsAddr
    , configLogOptions = logOptions
    }

defaultHaystackClient :: IO HaystackClient
defaultHaystackClient = defaultConfig >>= haystackClientFromConfig

haystackClientFromConfig :: Config -> IO HaystackClient
haystackClientFromConfig Config{..} = haystackClient configHaystackURL tlsManagerSettings configHostName configAppName

defaultStatsClient :: IO StatsClient
defaultStatsClient = defaultConfig >>= statsClientFromConfig

statsClientFromConfig :: Config -> IO StatsClient
statsClientFromConfig Config{..} = statsClient (addrHost configStatsAddr) (addrPort configStatsAddr) configAppName

lookupStatsAddr :: IO Addr
lookupStatsAddr = do
  addr <- lookupEnv "STATS_ADDR"
  let (host', port) = parseAddr (fmap ("statsd://" <>) addr)

  -- When running in Kubes, DOGSTATSD_HOST is set with the dogstatsd host.
  kubesHost <- lookupEnv "DOGSTATSD_HOST"
  let host = fromMaybe host' kubesHost

  pure (Addr host port)
  where
    defaultHost = "127.0.0.1"
    defaultPort = "28125"
    parseAddr a | Just s <- a
                , Just (Just (URIAuth _ host port)) <- uriAuthority <$> parseURI s
                = (parseHost host, parsePort port)
                | otherwise = (defaultHost, defaultPort)
    parseHost s = if null s then defaultHost else s
    parsePort s = if null s then defaultPort else dropWhile (':' ==) s
