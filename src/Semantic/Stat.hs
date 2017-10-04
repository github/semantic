module Semantic.Stat where


import Control.Concurrent.STM.TMQueue
import qualified Control.Concurrent.Async as Async
import GHC.Conc
import Data.List (intercalate)
import Data.Monoid

import System.IO

data Stat
  = Stat { name :: String
         , value :: String
         , tags :: [(String, String)]
         } deriving (Show)

increment :: String -> [(String, String)] -> Stat
increment n = Stat n "1"


queueStat :: TMQueue Stat -> Stat -> IO ()
queueStat q = atomically . writeTMQueue q

statSink :: TMQueue Stat -> IO ()
statSink q = do
  stat <- atomically (readTMQueue q)
  case stat of
    Just Stat{..} -> do
      hPutStrLn stderr $ name <> value <> fmtTags tags
      statSink q
    _ -> pure ()


-- data Metric = Gauge | Counter | Timer | Histogram | Set
--
fmtTags :: [(String, String)] -> String
fmtTags tags = intercalate "," (fmtTag <$> tags)

fmtTag :: (String, String) -> String
fmtTag (a, "") = a
fmtTag (a, b) = a ++ ":" ++ b
