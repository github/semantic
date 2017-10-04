module Semantic.Stat where


import Control.Concurrent.STM.TMQueue
import qualified Control.Concurrent.Async as Async
import GHC.Conc
import Data.List (intercalate)
import Data.Monoid
import Semantic.Queue

import System.IO

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

queueStat :: AsyncQ Stat () -> Stat -> IO ()
queueStat AsyncQ{..} = atomically . writeTMQueue queue

statSink :: () -> TMQueue Stat -> IO ()
statSink x q = do
  stat <- atomically (readTMQueue q)
  case stat of
    Just stat -> do
      hPutStrLn stderr $ renders stat ""
      statSink x q
    _ -> pure ()


-- Rendering

type RenderS = String -> String

class Render a where
  renders :: a -> RenderS

-- Instances

instance Render Stat where
  renders Stat{..}
    = renderString name
    . renderString "|"
    . renders value
    . renders tags

instance Render Metric where
  renders (Counter x) = renderString "c|" . renders x
  renders (Gauge x) = renderString "g|" . renders x
  renders (Histogram x) = renderString "h|" . renders x
  renders (Set x) = renderString "s|" . renders x
  renders (Timer x) = renderString "ms|" . renders x

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

renderString :: String -> RenderS
renderString = (<>)
