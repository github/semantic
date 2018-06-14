module Semantic.Stat
(
-- Primary API for creating stats.
  increment
, decrement
, count
, gauge
, timing
, withTiming
, histogram
, set
, Stat

-- Client
, defaultStatsClient
, statsClient
, StatsClient(..)
, closeStatClient

-- Internal, exposed for testing
, renderDatagram
, sendStat
) where


import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import           Data.List (intercalate)
import           Data.List.Split (splitOneOf)
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as Time (getCurrentTime)
import           Network.Socket
    (Socket (..), SocketType (..), addrAddress, addrFamily, close, connect, defaultProtocol, getAddrInfo, socket)
import           Network.Socket.ByteString
import           Network.URI
import           Numeric
import           Prologue
import           System.Environment
import           System.IO.Error

-- | A named piece of data you wish to record a specific 'Metric' for.
-- See https://docs.datadoghq.com/guides/dogstatsd/ for more details.
data Stat
  = Stat
  { statName  :: String  -- ^ Stat name, usually separated by '.' (e.g. "system.metric.name")
  , statValue :: Metric -- ^ 'Metric' value.
  , statTags  :: Tags    -- ^ Key/value 'Tags' (optional).
  }

-- | The various supported metric types in Datadog.
data Metric
  = Counter Int       -- ^ Counters track how many times something happens per second.
  | Gauge Double      -- ^ Gauges track the ebb and flow of a particular metric value over time.
  | Histogram Double  -- ^ Histograms calculate the statistical distribution of any kind of value.
  | Set Double        -- ^ Sets count the number of unique elements in a group
  | Timer Double      -- ^ Timers measure the amount of time a section of code takes to execute.

-- | Tags are key/value annotations. Values can blank.
type Tags = [(String, String)]


-- | Increment a counter.
increment :: String -> Tags -> Stat
increment n = count n 1

-- | Decrement a counter.
decrement :: String -> Tags -> Stat
decrement n = count n (-1)

-- | Arbitrary count.
count :: String -> Int -> Tags -> Stat
count n v = Stat n (Counter v)

-- | Arbitrary gauge value.
gauge :: String -> Double -> Tags -> Stat
gauge n v = Stat n (Gauge v)

-- | Timing in milliseconds.
timing :: String -> Double -> Tags -> Stat
timing n v = Stat n (Timer v)

-- | Run an IO Action and record timing
withTiming :: MonadIO io => String -> Tags -> io a -> io (a, Stat)
withTiming name tags action = do
  start <- liftIO Time.getCurrentTime
  result <- action
  end <- liftIO Time.getCurrentTime
  let duration = realToFrac (Time.diffUTCTime end start * 1000)
  pure (result, timing name duration tags)

-- | Histogram measurement.
histogram :: String -> Double -> Tags -> Stat
histogram n v = Stat n (Histogram v)

-- | Set counter.
set :: String -> Double -> Tags -> Stat
set n v = Stat n (Set v)

-- | Client for sending stats on a UDP socket.
data StatsClient
  = StatsClient
  { statsClientUDPSocket :: Socket
  , statsClientNamespace :: String
  , statsClientUDPHost   :: String
  , statsClientUDPPort   :: String
  }

-- | Create a default stats client. This function consults two optional
--   environment variables for the stats URI (default: 127.0.0.1:28125).
--     * STATS_ADDR     - String URI to send stats to in the form of `host:port`.
--     * DOGSTATSD_HOST - String hostname which will override the above host.
--                        Generally used on kubes pods.
defaultStatsClient :: MonadIO io => io StatsClient
defaultStatsClient = liftIO $ do
  addr <- lookupEnv "STATS_ADDR"
  let (host', port) = parseAddr (fmap ("statsd://" <>) addr)

  -- When running in Kubes, DOGSTATSD_HOST is set with the dogstatsd host.
  kubesHost <- lookupEnv "DOGSTATSD_HOST"
  let host = fromMaybe host' kubesHost

  statsClient host port "semantic"
  where
    defaultHost = "127.0.0.1"
    defaultPort = "28125"
    parseAddr a | Just s <- a
                , Just (Just (URIAuth _ host port)) <- uriAuthority <$> parseURI s
                = (parseHost host, parsePort port)
                | otherwise = (defaultHost, defaultPort)
    parseHost s = if null s then defaultHost else s
    parsePort s = if null s then defaultPort else dropWhile (':' ==) s


-- | Create a StatsClient at the specified host and port with a namespace prefix.
statsClient :: MonadIO io => String -> String -> String -> io StatsClient
statsClient host port statsClientNamespace = liftIO $ do
  (addr:_) <- getAddrInfo Nothing (Just host) (Just port)
  sock <- socket (addrFamily addr) Datagram defaultProtocol
  connect sock (addrAddress addr)
  pure (StatsClient sock statsClientNamespace host port)

-- | Close the client's underlying socket.
closeStatClient :: MonadIO io => StatsClient -> io ()
closeStatClient StatsClient{..} = liftIO (close statsClientUDPSocket)

-- | Send a stat over the StatsClient's socket.
sendStat :: MonadIO io => StatsClient -> Stat -> io ()
sendStat StatsClient{..} = liftIO . void . tryIOError . sendAll statsClientUDPSocket . B.pack . renderDatagram statsClientNamespace


-- Datagram Rendering

-- | Rendering of stats to their datagrams representations, which are packed and
-- sent over a socket.
class Render a where
  renders :: a -> RenderS

-- | A Function that prepends the output 'String' to an existing 'String'.
-- Analogous to 'ShowS'.
type RenderS = String -> String

-- | Utility function to prepend the string unchanged.
renderString :: String -> RenderS
renderString = (<>)

-- | Internal: Clean a stat name of reserved chars `|, @, :`
clean :: String -> String
clean = intercalate "_" . splitOneOf "|@:"

-- | Render a Stat (with namespace prefix) to a datagram String.
renderDatagram :: String -> Stat -> String
renderDatagram namespace stat = renderString prefix (renders stat "")
  where prefix | null namespace = ""
               | otherwise = clean namespace <> "."


-- Instances

instance Render Stat where
  renders Stat{..}
    = renderString (clean statName)
    . renderString ":"
    . renders statValue
    . renders statTags

instance Render Metric where
  renders (Counter x)   = renders x . renderString "|c"
  renders (Gauge x)     = renders x . renderString "|g"
  renders (Histogram x) = renders x . renderString "|h"
  renders (Set x)       = renders x . renderString "|s"
  renders (Timer x)     = renders x . renderString "|ms"

instance Render Tags where
  renders [] = renderString ""
  renders xs = renderString "|#" . (\x -> x <> intercalate "," (renderTag <$> xs))
    where
      renderTag (k, "") = k
      renderTag (k, v)  = k <> ":" <> v

instance Render Int where
  renders = shows

instance Render Double where
  renders = showFFloat (Just 5)
