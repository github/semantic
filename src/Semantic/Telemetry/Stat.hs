{-# LANGUAGE FlexibleInstances, RecordWildCards #-}
module Semantic.Telemetry.Stat
(
-- Primary API for creating stats.
  increment
, decrement
, count
, gauge
, timing
, withTiming
, withTiming'
, histogram
, set
, Stat
, Tags
, Host
, Port
, Namespace

-- Client
, statsClient
, StatsClient(..)
, closeStatClient

-- Internal, exposed for testing
, renderDatagram
, sendStat
) where


import qualified Data.ByteString.Char8 as B
import           Data.List (intercalate)
import           Data.List.Split (splitOneOf)
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as Time (getCurrentTime)
import           Network.Socket
    (Socket (..), SocketType (..), addrAddress, addrFamily, close, connect, defaultProtocol, getAddrInfo, socket)
import           Network.Socket.ByteString
import           Numeric
import           Prologue
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

-- | Run an IO Action and record timing in a Stat.
withTiming :: MonadIO io => String -> Tags -> io a -> io (a, Stat)
withTiming name tags action = do
  (res, duration) <- withTiming' action
  pure (res, timing name duration tags)

-- | Run an IO Action and record timing.
withTiming' :: MonadIO io => io a -> io (a, Double)
withTiming' action = do
  start <- liftIO Time.getCurrentTime
  result <- action
  end <- liftIO Time.getCurrentTime
  let duration = realToFrac (Time.diffUTCTime end start * 1000)
  pure (result, duration)

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
  , statsClientUDPHost   :: Host
  , statsClientUDPPort   :: Port
  }

type Host = String
type Port = String
type Namespace = String

-- | Create a StatsClient at the specified host and port with a namespace prefix.
statsClient :: MonadIO io => Host -> Port -> Namespace -> io StatsClient
statsClient host port ns = liftIO $ do
  (addr:_) <- getAddrInfo Nothing (Just host) (Just port)
  sock <- socket (addrFamily addr) Datagram defaultProtocol
  connect sock (addrAddress addr)
  pure (StatsClient sock ns host port)

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
