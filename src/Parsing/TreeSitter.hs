{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeOperators #-}
module Parsing.TreeSitter
( parseToAST
) where

import Prologue

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.AST (AST, Node (Node))
import Data.Blob
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Range
import Data.Source
import Data.Span
import Data.Term
import Foreign hiding (void)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Array (allocaArray)

import qualified TreeSitter.Language as TS
import qualified TreeSitter.Node as TS
import qualified TreeSitter.Parser as TS
import qualified TreeSitter.Tree as TS

parserTimeout :: Int -- in microseconds
parserTimeout = 5000000

-- Change this to putStrLn if you want to debug the locking/cancellation code.
dbg :: String -> IO ()
dbg _ = return ()

data ParserStatus
  = Preflight
  | InProgress
  | Completed
  | Cancelled
    deriving (Eq, Show)

-- | Parse 'Source' with the given 'TS.Language' and return its AST.
-- Returns Nothing if the operation timed out.
parseToAST :: (Bounded grammar, Enum grammar) => Ptr TS.Language -> Blob -> IO (Maybe (AST [] grammar))
parseToAST language Blob{..} = bracket TS.ts_parser_new TS.ts_parser_delete $ \ parser -> do
  -- MVar that keeps track of the parser's cancellation state.
  status <- newMVar Preflight
  let setStatus = void . swapMVar status

  -- A lock to ensure two threads don't modify 'status' at the same time.
  -- We could avoid this lock if we used STM here, but we can't, because we need
  -- to call ts_parser_enabled inside a critical section, which is in IO.
  -- Nor can we use 'timeout', since that won't cancel in-flight FFI calls.
  lock <- newMVar ()
  -- A helper to isolate critical sections.
  let withLock :: IO a -> IO a
      withLock go = bracket
        (dbg "Taking lock" *> takeMVar lock)
        (const (dbg "Releasing lock" *> putMVar lock ()))
        (const go)

  TS.ts_parser_halt_on_error parser (CBool 1)
  TS.ts_parser_set_language parser language

  -- Kick off an asynchronous thread that waits for 'parserTimeout' microseconds
  -- and cancels the parser if the status is still 'InProgress'.
  watchdog <- async $ do
    dbg "Starting watchdog"
    threadDelay parserTimeout -- wait
    dbg "Watchdog finished"
    withLock $ do
      current <- readMVar status
      dbg ("Got value " <> show current)
      when (current /= Completed) $ do
        setStatus Cancelled
        dbg "Cancelling"
        TS.ts_parser_set_enabled parser (CBool 0)
        enabled <- TS.ts_parser_enabled parser
        dbg ("Watchdog: parser enabled: " <> show enabled)

  unsafeUseAsCStringLen (sourceBytes blobSource) $ \ (source, len) -> do
    alloca (\ rootPtr -> do
      let acquire = do
            dbg "Starting parse"
            withLock (setStatus InProgress)
            -- Uncomment this if you want to test out cancellation
            -- threadDelay 6000000
            enabled <- TS.ts_parser_enabled parser
            dbg ("Parser enabled: " <> show enabled)
            TS.ts_parser_parse_string parser nullPtr source len

      let release t
            | t == nullPtr = dbg "Parse failed"
            | otherwise = do
                dbg "Parse completed"
                cancel watchdog
                setStatus Completed
                TS.ts_tree_delete t

      let go treePtr = do
            if treePtr == nullPtr
              then pure Nothing
              else do
                TS.ts_tree_root_node_p treePtr rootPtr
                fmap Just (peek rootPtr >>= anaM toAST)
      bracket acquire release go)


toAST :: forall grammar . (Bounded grammar, Enum grammar) => TS.Node -> IO (Base (AST [] grammar) TS.Node)
toAST node@TS.Node{..} = do
  let count = fromIntegral nodeChildCount
  children <- allocaArray count $ \ childNodesPtr -> do
    _ <- with nodeTSNode (\ nodePtr -> TS.ts_node_copy_child_nodes nodePtr childNodesPtr (fromIntegral count))
    peekArray count childNodesPtr
  pure $! In (Node (toEnum (min (fromIntegral nodeSymbol) (fromEnum (maxBound :: grammar)))) (nodeRange node) (nodeSpan node)) children

anaM :: (Corecursive t, Monad m, Traversable (Base t)) => (a -> m (Base t a)) -> a -> m t
anaM g = a where a = pure . embed <=< traverse a <=< g


nodeRange :: TS.Node -> Range
nodeRange TS.Node{..} = Range (fromIntegral nodeStartByte) (fromIntegral nodeEndByte)

nodeSpan :: TS.Node -> Span
nodeSpan TS.Node{..} = nodeStartPoint `seq` nodeEndPoint `seq` Span (pointPos nodeStartPoint) (pointPos nodeEndPoint)
  where pointPos TS.TSPoint{..} = pointRow `seq` pointColumn `seq` Pos (1 + fromIntegral pointRow) (1 + fromIntegral pointColumn)
