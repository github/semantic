{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeOperators #-}
module Parsing.TreeSitter
( Timeout (..)
, parseToAST
) where

import Prologue

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
import System.Timeout

import qualified TreeSitter.Language as TS
import qualified TreeSitter.Node as TS
import qualified TreeSitter.Parser as TS
import qualified TreeSitter.Tree as TS

newtype Timeout = Seconds Int

-- Change this to putStrLn if you want to debug the locking/cancellation code.
dbg :: String -> IO ()
dbg = const (pure ())

runParser :: (Enum grammar, Bounded grammar) => Ptr TS.Parser -> Source -> IO (Maybe (AST [] grammar))
runParser parser blobSource  = unsafeUseAsCStringLen (sourceBytes blobSource) $ \ (source, len) -> do
    alloca (\ rootPtr -> do
      let acquire = do
            dbg "Starting parse"
            -- Change this to TS.ts_parser_loop_until_cancelled if you want to test out cancellation
            TS.ts_parser_parse_string parser nullPtr source len

      let release t
            | t == nullPtr = dbg "Parse failed"
            | otherwise = dbg "Parse completed" *> TS.ts_tree_delete t

      let go treePtr = do
            if treePtr == nullPtr
              then pure Nothing
              else do
                TS.ts_tree_root_node_p treePtr rootPtr
                fmap Just (peek rootPtr >>= anaM toAST)
      bracket acquire release go)

-- | Parse 'Source' with the given 'TS.Language' and return its AST.
-- Returns Nothing if the operation timed out.
parseToAST :: (Bounded grammar, Enum grammar) => Timeout -> Ptr TS.Language -> Blob -> IO (Maybe (AST [] grammar))
parseToAST (Seconds s) language Blob{..} = bracket TS.ts_parser_new TS.ts_parser_delete $ \ parser -> do
  let parserTimeout = s * 1000000

  TS.ts_parser_halt_on_error parser (CBool 1)
  TS.ts_parser_set_language parser language

  parsing <- async (runParser parser blobSource)

  -- Kick the parser off asynchronously and wait according to the provided timeout.
  res <- timeout parserTimeout (wait parsing)

  -- If we get a Nothing back, then we failed, so we need to disable the parser, which
  -- will let the call to runParser terminate, cleaning up appropriately
  when (isNothing res) (TS.ts_parser_set_enabled parser (CBool 0))

  pure (join res)


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
