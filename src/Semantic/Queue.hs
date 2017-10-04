module Semantic.Queue where

import Control.Concurrent.STM.TMQueue
import Control.Concurrent.Async as Async
import GHC.Conc

data AsyncQ a b
  = AsyncQ
  { queue :: TMQueue a
  , sink :: Async ()
  , extra :: b
  }

newQueue :: b -> (b -> TMQueue a -> IO ()) -> IO (AsyncQ a b)
newQueue b f = do
  q <- newTMQueueIO
  sink <- Async.async (f b q)
  pure (AsyncQ q sink b)

closeQueue :: AsyncQ a b -> IO ()
closeQueue AsyncQ{..} = do
  atomically (closeTMQueue queue)
  Async.wait sink
