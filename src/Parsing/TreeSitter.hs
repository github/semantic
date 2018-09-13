{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeOperators #-}
module Parsing.TreeSitter
( Duration(..)
, parseToAST
) where

import Prologue hiding (bracket)

import           Control.Concurrent.Async
import qualified Control.Exception as Exc (bracket)
import           Control.Monad.Effect
import           Control.Monad.Effect.Exception
import           Control.Monad.Effect.Trace
import           Control.Monad.IO.Class
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Foreign
import           Foreign.C.Types (CBool (..))
import           Foreign.Marshal.Array (allocaArray)
import           System.Timeout

import Data.AST (AST, Node (Node))
import Data.Blob
import Data.Duration
import Data.Range
import Data.Source
import Data.Span
import Data.Term

import qualified TreeSitter.Language as TS
import qualified TreeSitter.Node as TS
import qualified TreeSitter.Parser as TS
import qualified TreeSitter.Tree as TS

data Result grammar
  = Failed
  | Succeeded (AST [] grammar)

runParser :: (Enum grammar, Bounded grammar) => Ptr TS.Parser -> Source -> IO (Result grammar)
runParser parser blobSource  = unsafeUseAsCStringLen (sourceBytes blobSource) $ \ (source, len) -> do
    alloca (\ rootPtr -> do
      let acquire = do
            -- Change this to TS.ts_parser_loop_until_cancelled if you want to test out cancellation
            TS.ts_parser_parse_string parser nullPtr source len

      let release t
            | t == nullPtr = pure ()
            | otherwise = TS.ts_tree_delete t

      let go treePtr = do
            if treePtr == nullPtr
              then pure Failed
              else do
                TS.ts_tree_root_node_p treePtr rootPtr
                ptr <- peek rootPtr
                Succeeded <$> anaM toAST ptr
      Exc.bracket acquire release go)

-- | Parse 'Source' with the given 'TS.Language' and return its AST.
-- Returns Nothing if the operation timed out.
parseToAST :: (Bounded grammar, Enum grammar, Member (Lift IO) effects, Member Trace effects, PureEffects effects) => Duration -> Ptr TS.Language -> Blob -> Eff effects (Maybe (AST [] grammar))
parseToAST parseTimeout language Blob{..} = bracket TS.ts_parser_new TS.ts_parser_delete $ \ parser -> do
  liftIO $ do
    TS.ts_parser_halt_on_error parser (CBool 1)
    TS.ts_parser_set_language parser language

  trace $ "tree-sitter: beginning parsing " <> blobPath

  parsing <- liftIO . async $ runParser parser blobSource

  -- Kick the parser off asynchronously and wait according to the provided timeout.
  res <- liftIO . timeout (toMicroseconds parseTimeout) $ wait parsing

  case res of
    Just Failed          -> Nothing  <$ trace ("tree-sitter: parsing failed " <> blobPath)
    Just (Succeeded ast) -> Just ast <$ trace ("tree-sitter: parsing succeeded " <> blobPath)
    Nothing -> do
      trace $ "tree-sitter: parsing timed out " <> blobPath
      Nothing <$ liftIO (TS.ts_parser_set_enabled parser (CBool 0))


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
