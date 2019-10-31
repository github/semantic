{-# LANGUAGE RecordWildCards #-}
module Semantic.Telemetry.AsyncQueue
(
  AsyncQueue(..)
, newAsyncQueue
, newAsyncQueue'
, writeAsyncQueue
, closeAsyncQueue
)
where

import Control.Concurrent.Async as Async
import Control.Concurrent.STM.TBMQueue
import Control.Monad
import GHC.Conc

-- | 'AsyncQueue' represents a 'TBMQueue' that's drained from a separate thread.
-- It is intended to be used to queue data from a pure function and then process
-- that data in IO on a separate thread. 'AsyncQueue' is parameterized by:
--   * 'a'     - the type of message stored on the queue.
--   * 'extra' - any other type needed to process messages on the queue.
data AsyncQueue a extra
  = AsyncQueue
  { asyncQueue      :: TBMQueue a -- ^ The underlying 'TBMQueue'.
  , asyncQueueSink  :: Async ()   -- ^ A sink that will drain the queue.
  , asyncQueueExtra :: extra      -- ^ Any exta data the queue needs to use.
  }

-- | Create a new AsyncQueue with the given capacity using the defaultSink.
newAsyncQueue :: Int -> (extra -> a -> IO ()) -> extra -> IO (AsyncQueue a extra)
newAsyncQueue i = newAsyncQueue' i . defaultSink

-- | Create a new AsyncQueue with the given capacity, specifying a custom sink.
newAsyncQueue' :: Int -> (extra -> TBMQueue a -> IO ()) -> extra -> IO (AsyncQueue a extra)
newAsyncQueue' i f extra = do
  q <- newTBMQueueIO i
  s <- Async.async (f extra q)
  pure (AsyncQueue q s extra)

-- | Write a message to the queue.
writeAsyncQueue :: AsyncQueue a extra -> a -> IO ()
writeAsyncQueue AsyncQueue{..} = void . atomically . tryWriteTBMQueue asyncQueue

-- | Drain messages from the queue, calling the specified function for each message.
defaultSink :: (extra -> a -> IO ()) -> extra -> TBMQueue a -> IO ()
defaultSink f extra q = do
  msg <- atomically (readTBMQueue q)
  maybe (pure ()) go msg
  where go msg = f extra msg >> defaultSink f extra q

-- | Close the queue.
closeAsyncQueue :: AsyncQueue a extra -> IO ()
closeAsyncQueue AsyncQueue{..} = do
  atomically (closeTBMQueue asyncQueue)
  Async.wait asyncQueueSink
