module Semantic.Queue where

import Control.Concurrent.STM.TMQueue
import qualified Control.Concurrent.Async as Async
import GHC.Conc


newQueue :: (TMQueue a -> IO ()) -> IO (TMQueue a, Async.Async ())
newQueue f = do
  q <- newTMQueueIO
  sink <- Async.async (f q)
  pure (q, sink)

closeQueue :: TMQueue a -> Async.Async () -> IO ()
closeQueue q sink = do
  atomically (closeTMQueue q)
  Async.wait sink
