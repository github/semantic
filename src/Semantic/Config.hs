module Semantic.Config where

import           Control.Exception
import           Network.BSD
import           Network.HTTP.Client.TLS
import           Network.URI
import           Parsing.TreeSitter (Timeout (..))
import           Prologue
import           Semantic.Env
import           Semantic.Telemetry
import           Semantic.Telemetry.AsyncQueue
import qualified Semantic.Telemetry.Haystack as Haystack
import qualified Semantic.Telemetry.Log as Log
import qualified Semantic.Telemetry.Stat as Stat
import           Semantic.Version
import           System.Environment
import           System.IO (hIsTerminalDevice, stderr)
import           System.Posix.Process
import           System.Posix.Types

data Config
  = Config
  { configAppName                :: String       -- ^ Application name (semantic)
  , configHostName               :: String       -- ^ HostName from getHostName
  , configProcessID              :: ProcessID    -- ^ ProcessID from getProcessID
  , configHaystackURL            :: Maybe String -- ^ URL of Haystack (with creds) from environment
  , configStatsAddr              :: StatsAddr    -- ^ Address of statsd/datadog

  , configTreeSitterParseTimeout :: Timeout      -- ^ Timeout in milliseconds before canceling tree-sitter parsing
  , configMaxTelemetyQueueSize   :: Int          -- ^ Max size of telemetry queues before messages are dropped.
  , configIsTerminal             :: Bool         -- ^ Whether a terminal is attached (set automaticaly at runtime).
  , configLogPrintSource         :: Bool         -- ^ Whether to print the source reference when logging errors (set automatically at runtime).
  , configLogFormatter           :: LogFormatter -- ^ Log formatter to use (set automaticaly at runtime).

  , configOptions                :: Options      -- ^ Options configurable via command line arguments.
  }

-- Options configurable via command line arguments.
data Options
  = Options
  { optionsLogLevel      :: Maybe Level   -- ^ What level of messages to log. 'Nothing' disabled logging.
  , optionsRequestID     :: Maybe String  -- ^ Optional request id for tracing across systems.
  , optionsFailOnWarning :: Bool          -- ^ Should semantic fail fast on assignment warnings (for testing)
  }

data StatsAddr = StatsAddr { addrHost :: String, addrPort :: String }

defaultOptions :: Options
defaultOptions = Options (Just Warning) Nothing False

defaultConfig :: IO Config
defaultConfig = defaultConfig' defaultOptions

defaultConfig' :: Options -> IO Config
defaultConfig' options@Options{..} = do
  pid <- getProcessID
  hostName <- getHostName
  isTerminal <- hIsTerminalDevice stderr
  haystackURL <- lookupEnv "HAYSTACK_URL"
  statsAddr <- lookupStatsAddr
  size <- envLookupInt 1000 "MAX_TELEMETRY_QUEUE_SIZE"
  parseTimeout <- envLookupInt 10000 "TREE_SITTER_PARSE_TIMEOUT" -- Default is 10 seconds
  pure Config
    { configAppName = "semantic"
    , configHostName = hostName
    , configProcessID = pid
    , configHaystackURL = haystackURL
    , configStatsAddr = statsAddr

    , configTreeSitterParseTimeout = Milliseconds parseTimeout
    , configMaxTelemetyQueueSize = size
    , configIsTerminal = isTerminal
    , configLogPrintSource = isTerminal
    , configLogFormatter = if isTerminal then terminalFormatter else logfmtFormatter

    , configOptions = options
    }


defaultHaystackFromConfig :: Config -> Haystack.ErrorLogger IO -> IO HaystackQueue
defaultHaystackFromConfig c@Config{..} logError = haystackClientFromConfig c >>= newAsyncQueue configMaxTelemetyQueueSize (Haystack.reportError logError)

haystackClientFromConfig :: Config -> IO HaystackClient
haystackClientFromConfig Config{..} = haystackClient configHaystackURL tlsManagerSettings configAppName


withLogger :: Config -> (LogQueue -> IO c) -> IO c
withLogger c = bracket (defaultLoggerFromConfig c) closeAsyncQueue

defaultLoggerFromConfig :: Config -> IO LogQueue
defaultLoggerFromConfig Config{..} =
  newAsyncQueue configMaxTelemetyQueueSize Log.writeLogMessage LogOptions {
    logOptionsLevel     = optionsLogLevel configOptions
  , logOptionsFormatter = configLogFormatter
  , logOptionsContext   =
    [ ("app", configAppName)
    , ("pid", show configProcessID)
    , ("hostname", configHostName)
    , ("sha", buildSHA)
    ] <> [("request_id", x) | x <- toList (optionsRequestID configOptions) ]
  }

withStatter :: Config -> (StatQueue -> IO c) -> IO c
withStatter c = bracket (defaultStatterFromConfig c) $ \statter -> do
  closeAsyncQueue statter
  Stat.closeStatClient (asyncQueueExtra statter)

defaultStatterFromConfig :: Config -> IO StatQueue
defaultStatterFromConfig c@Config{..} = statsClientFromConfig c >>= newAsyncQueue configMaxTelemetyQueueSize Stat.sendStat

statsClientFromConfig :: Config -> IO StatsClient
statsClientFromConfig Config{..} = statsClient (addrHost configStatsAddr) (addrPort configStatsAddr) configAppName

lookupStatsAddr :: IO StatsAddr
lookupStatsAddr = do
  addr <- lookupEnv "STATS_ADDR"
  let (host', port) = parseAddr (fmap ("statsd://" <>) addr)

  -- When running in Kubes, DOGSTATSD_HOST is set with the dogstatsd host.
  kubesHost <- lookupEnv "DOGSTATSD_HOST"
  let host = fromMaybe host' kubesHost

  pure (StatsAddr host port)
  where
    defaultHost = "127.0.0.1"
    defaultPort = "28125"
    parseAddr a | Just s <- a
                , Just (Just (URIAuth _ host port)) <- uriAuthority <$> parseURI s
                = (parseHost host, parsePort port)
                | otherwise = (defaultHost, defaultPort)
    parseHost s = if null s then defaultHost else s
    parsePort s = if null s then defaultPort else dropWhile (':' ==) s
