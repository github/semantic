module Semantic.Stat where

import Control.Concurrent.STM.TMQueue
import GHC.Conc
import Data.List (intercalate)
import Data.Monoid
import Semantic.Queue

import System.IO
import System.IO.Error
import Network.Socket (Socket(..), SocketType(..), socket, connect, getAddrInfo, addrFamily, addrAddress, defaultProtocol)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B

data Metric
  = Counter Int
  | Gauge Double
  | Histogram Double
  | Set Double
  | Timer Double

type Tag = (String, String)
type Tags = [Tag]

data Stat
  = Stat { name :: String
         , value :: Metric
         , tags :: Tags
         }

count :: String -> Int -> Tags -> Stat
count n v = Stat n (Counter v)

increment :: String -> Tags -> Stat
increment n = count n 1

decrement :: String -> Tags -> Stat
decrement n = count n (-1)


data UdpClient
  = UdpClient
  { udpSocket :: Socket
  , udpNamespace :: String
  }

statsClient :: String -> Int -> IO UdpClient
statsClient host port = do
  (addr:_) <- getAddrInfo Nothing (Just host) (Just (show port))
  sock <- socket (addrFamily addr) Datagram defaultProtocol
  connect sock (addrAddress addr)
  pure (UdpClient sock "")

sendStats :: UdpClient -> String -> IO ()
sendStats UdpClient{..} d = do
  res <- tryIOError (sendAll udpSocket msg)
  pure $ either (const ()) id res
  where msg = B.pack (prefix <> d)
        prefix | null udpNamespace = ""
               | otherwise = udpNamespace <> "."


queueStat :: AsyncQ Stat UdpClient -> Stat -> IO ()
queueStat AsyncQ{..} = atomically . writeTMQueue queue

statSink :: UdpClient -> TMQueue Stat -> IO ()
statSink client q = do
  stat <- atomically (readTMQueue q)
  case stat of
    Just stat -> do
      hPutStrLn stderr $ renders stat ""
      _ <- sendStats client (renders stat "")
      statSink client q
    _ -> pure ()


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
