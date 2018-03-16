{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators #-}
module Parsing.TreeSitter
( parseToAST
) where

import Prologue
import Data.AST (AST, Node(Node))
import Data.Blob
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Range
import Data.Source
import Data.Span
import Data.Term
import Foreign
import Foreign.Marshal.Array (allocaArray)
import qualified TreeSitter.Document as TS
import qualified TreeSitter.Node as TS
import qualified TreeSitter.Language as TS

-- | Parse 'Source' with the given 'TS.Language' and return its AST.
parseToAST :: (Bounded grammar, Enum grammar) => Ptr TS.Language -> Blob -> IO (AST [] grammar)
parseToAST language Blob{..} = bracket TS.ts_document_new TS.ts_document_free $ \ document -> do
  TS.ts_document_set_language document language
  root <- unsafeUseAsCStringLen (sourceBytes blobSource) $ \ (source, len) -> do
    TS.ts_document_set_input_string_with_length document source len
    TS.ts_document_parse_halt_on_error document
    alloca (\ rootPtr -> do
      TS.ts_document_root_node_p document rootPtr
      peek rootPtr)

  anaM toAST root

toAST :: forall grammar . (Bounded grammar, Enum grammar) => TS.Node -> IO (Base (AST [] grammar) TS.Node)
toAST node@TS.Node{..} = do
  let count = fromIntegral nodeChildCount
  children <- allocaArray count $ \ childNodesPtr -> do
    _ <- with nodeTSNode (\ nodePtr -> TS.ts_node_copy_child_nodes nullPtr nodePtr childNodesPtr (fromIntegral count))
    peekArray count childNodesPtr
  pure $! In (Node (toEnum (min (fromIntegral nodeSymbol) (fromEnum (maxBound :: grammar)))) (nodeRange node) (nodeSpan node)) children

anaM :: (Corecursive t, Monad m, Traversable (Base t)) => (a -> m (Base t a)) -> a -> m t
anaM g = a where a = pure . embed <=< traverse a <=< g


nodeRange :: TS.Node -> Range
nodeRange TS.Node{..} = Range (fromIntegral nodeStartByte) (fromIntegral nodeEndByte)

nodeSpan :: TS.Node -> Span
nodeSpan TS.Node{..} = nodeStartPoint `seq` nodeEndPoint `seq` Span (pointPos nodeStartPoint) (pointPos nodeEndPoint)
  where pointPos TS.TSPoint{..} = pointRow `seq` pointColumn `seq` Pos (1 + fromIntegral pointRow) (1 + fromIntegral pointColumn)
