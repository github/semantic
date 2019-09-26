{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeOperators #-}
module Parsing.TreeSitter
( Duration(..)
, parseToAST
) where

import Prologue hiding (bracket)

import           Control.Effect.Resource
import           Control.Effect.Trace
import qualified Control.Exception as Exc (bracket)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Foreign
import           Foreign.C.Types (CBool (..))
import           Foreign.Marshal.Array (allocaArray)

import           Data.AST (AST, Node (Node))
import           Data.Blob
import           Data.Duration
import           Data.Term
import           Source.Loc
import           Source.Source (Source)
import qualified Source.Source as Source
import           Source.Span

import qualified TreeSitter.Language as TS
import qualified TreeSitter.Node as TS
import qualified TreeSitter.Parser as TS
import qualified TreeSitter.Tree as TS

runParserToAST :: (Enum grammar, Bounded grammar) => Ptr TS.Parser -> Source -> IO (Maybe (AST [] grammar))
runParserToAST parser blobSource = unsafeUseAsCStringLen (Source.bytes blobSource) $ \ (source, len) ->
  alloca (\ rootPtr ->
    let acquire =
          -- Change this to TS.ts_parser_loop_until_cancelled if you want to test out cancellation
          TS.ts_parser_parse_string parser nullPtr source len

        release t
          | t == nullPtr = pure ()
          | otherwise = TS.ts_tree_delete t

        go treePtr = if treePtr == nullPtr then
          pure Nothing
        else do
          TS.ts_tree_root_node_p treePtr rootPtr
          ptr <- peek rootPtr
          Just <$> anaM toAST ptr
    in Exc.bracket acquire release go)

-- | Parse 'Source' with the given 'TS.Language' and return its AST.
-- Returns Nothing if the operation timed out.
parseToAST :: ( Bounded grammar
              , Carrier sig m
              , Enum grammar
              , Member Resource sig
              , Member Trace sig
              , MonadIO m
              )
           => Duration
           -> Ptr TS.Language
           -> Blob
           -> m (Maybe (AST [] grammar))
parseToAST parseTimeout language b@Blob{..} = bracket (liftIO TS.ts_parser_new) (liftIO . TS.ts_parser_delete) $ \ parser -> do
  compatible <- liftIO $ do
    let timeoutMicros = fromIntegral $ toMicroseconds parseTimeout
    TS.ts_parser_set_timeout_micros parser timeoutMicros
    TS.ts_parser_halt_on_error parser (CBool 1)
    TS.ts_parser_set_language parser language
  result <- if compatible then
    liftIO $ runParserToAST parser blobSource
  else
    Nothing <$ trace "tree-sitter: incompatible versions"
  case result of
    Nothing  -> Nothing  <$ trace ("tree-sitter: parsing failed " <> blobPath b)
    Just ast -> Just ast <$ trace ("tree-sitter: parsing succeeded " <> blobPath b)

toAST :: forall grammar . (Bounded grammar, Enum grammar) => TS.Node -> IO (Base (AST [] grammar) TS.Node)
toAST node@TS.Node{..} = do
  let count = fromIntegral nodeChildCount
  children <- allocaArray count $ \ childNodesPtr -> do
    _ <- with nodeTSNode (`TS.ts_node_copy_child_nodes` childNodesPtr)
    peekArray count childNodesPtr
  pure $! In (Node (toEnum (min (fromIntegral nodeSymbol) (fromEnum (maxBound :: grammar)))) (Loc (nodeRange node) (nodeSpan node))) children

anaM :: (Corecursive t, Monad m, Traversable (Base t)) => (a -> m (Base t a)) -> a -> m t
anaM g = a where a = pure . embed <=< traverse a <=< g


nodeRange :: TS.Node -> Range
nodeRange TS.Node{..} = Range (fromIntegral nodeStartByte) (fromIntegral nodeEndByte)

nodeSpan :: TS.Node -> Span
nodeSpan TS.Node{..} = nodeStartPoint `seq` nodeEndPoint `seq` Span (pointPos nodeStartPoint) (pointPos nodeEndPoint)
  where pointPos TS.TSPoint{..} = pointRow `seq` pointColumn `seq` Pos (1 + fromIntegral pointRow) (1 + fromIntegral pointColumn)
