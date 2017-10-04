module Semantic.Queue where

import Control.Concurrent.Async as Async
import Control.Concurrent.STM.TMQueue
import GHC.Conc

-- Represents a TMQueue that's drained from a separate thread.
data AsyncQ a b
  = AsyncQ
  { queue :: TMQueue a -- The queue.
  , sink :: Async ()   -- A sink that will drain the queue.
  , extra :: b         -- Any extra data the queue needs to use.
  }

-- Create a new AsyncQ.
newQueue :: (b -> TMQueue a -> IO ()) -> b-> IO (AsyncQ a b)
newQueue f b = do
  q <- newTMQueueIO
  sink <- Async.async (f b q)
  pure (AsyncQ q sink b)

-- Close the queue.
closeQueue :: AsyncQ a b -> IO ()
closeQueue AsyncQ{..} = do
  atomically (closeTMQueue queue)
  Async.wait sink
