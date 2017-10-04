module Semantic.Stat
(
-- Primary API for creating stats.
  increment
, decrement
, count
, gauge
, timing
, histogram
, set
, Stat

, queueStat -- Queue a Stat to be sent
, statSink  -- Sink the queue to the network

-- Client
, defaultStatsClient
, StatClient
) where


import Control.Arrow ((&&&))
import Control.Concurrent.STM.TMQueue
import Data.Functor
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import GHC.Conc
import Network.Socket (Socket(..), SocketType(..), socket, connect, getAddrInfo, addrFamily, addrAddress, defaultProtocol)
import Network.Socket.ByteString
import Network.URI
import qualified Data.ByteString.Char8 as B
import System.Environment
import System.IO.Error

import Semantic.Queue

data Stat
  = Stat { name :: String
         , value :: Metric
         , tags :: Tags
         }

data Metric
  = Counter Int       -- Counters track how many times something happens per second.
  | Gauge Double      -- Gauges track the ebb and flow of a particular metric value over time.
  | Histogram Double  -- Histograms calculate the statistical distribution of any kind of value.
  | Set Double        -- Sets count the number of unique elements in a group
  | Timer Double      -- Timers measure the amount of time a section of code takes to execute.

type Tag = (String, String)

type Tags = [Tag]


-- Increment a counter.
increment :: String -> Tags -> Stat
increment n = count n 1

-- Decrement a counter.
decrement :: String -> Tags -> Stat
decrement n = count n (-1)

-- Arbitrary count.
count :: String -> Int -> Tags -> Stat
count n v = Stat n (Counter v)

-- Arbitrary gauge value.
gauge :: String -> Double -> Tags -> Stat
gauge n v = Stat n (Gauge v)

-- Timing in milliseconds.
timing :: String -> Double -> Tags -> Stat
timing n v = Stat n (Timer v)

-- Histogram measurement.
histogram :: String -> Double -> Tags -> Stat
histogram n v = Stat n (Histogram v)

-- Set counter.
set :: String -> Double -> Tags -> Stat
set n v = Stat n (Set v)

data StatClient
  = StatClient
  { udpSocket :: Socket
  , namespace :: String
  }

-- Create a default stats client. This function consults two optional
-- environment variables for the stats URI (default: 127.0.0.1:28125).
--   * STATS_ADDR     - String URI to send stats to in the form of `host:port`.
--   * DOGSTATSD_HOST - String hostname which will override the above host.
--                      Generally used on kubes pods.
defaultStatsClient :: IO StatClient
defaultStatsClient = do
  addr <- lookupEnv "STATS_ADDR"
  let (host', port) = maybe defaultHostPort parseAddr addr

  -- When running in Kubes, DOGSTATSD_HOST is set with the dogstatsd host.
  kubesHost <- lookupEnv "DOGSTATSD_HOST"
  let host = fromMaybe host' kubesHost

  statsClient host port "semantic"
  where
    defaultHostPort = ("127.0.0.1", "28125")
    parseAddr = maybe defaultHostPort parseAuthority . parseURI
    parseAuthority = maybe defaultHostPort (uriRegName &&& uriPort) . uriAuthority


-- Create a StatsClient at the specified host and port with a namespace prefix.
statsClient :: String -> String -> String -> IO StatClient
statsClient host port namespace = do
  (addr:_) <- getAddrInfo Nothing (Just host) (Just port)
  sock <- socket (addrFamily addr) Datagram defaultProtocol
  connect sock (addrAddress addr)
  pure (StatClient sock namespace)

-- Send a stat over the StatClient's socket.
sendStats :: StatClient -> Stat -> IO ()
sendStats StatClient{..} = void . tryIOError . sendAll udpSocket . B.pack . datagram
  where datagram stat = renderString prefix (renders stat "")
        prefix | null namespace = ""
               | otherwise = namespace <> "."


-- Queue a stat to be sent.
queueStat :: AsyncQ Stat StatClient -> Stat -> IO ()
queueStat AsyncQ{..} = atomically . writeTMQueue queue

-- Drains stat messages from the queue and sends those stats over the configured
-- UDP socket. Intended to be run in a dedicated thread.
statSink :: StatClient -> TMQueue Stat -> IO ()
statSink client q = do
  stat <- atomically (readTMQueue q)
  maybe (pure ()) send stat
  where send stat = sendStats client stat >> statSink client q


-- Datagram Rendering

type RenderS = String -> String

class Render a where
  renders :: a -> RenderS

renderString :: String -> RenderS
renderString = (<>)


-- Instances

instance Render Stat where
  renders Stat{..}
    = renderString name
    . renderString "|"
    . renders value
    . renders tags

instance Render Metric where
  renders (Counter x)   = renderString "c|"  . renders x
  renders (Gauge x)     = renderString "g|"  . renders x
  renders (Histogram x) = renderString "h|"  . renders x
  renders (Set x)       = renderString "s|"  . renders x
  renders (Timer x)     = renderString "ms|" . renders x

instance Render Tags where
  renders [] = renderString ""
  renders xs = renderString "|#" . (\x -> x <> intercalate "," (renderTag <$> xs))
    where
      renderTag (k, "") = k
      renderTag (k, v) = k <> ":" <> v

instance Render Int where
  renders = shows

instance Render Double where
  renders = shows
