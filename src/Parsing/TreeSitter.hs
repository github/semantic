{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeOperators #-}
module Parsing.TreeSitter
( Duration(..)
, parseToAST
) where

import Prologue

import           Control.Effect.Trace
import           Foreign
import           Foreign.C.Types (CBool (..))
import           Foreign.Marshal.Array (allocaArray)

import           Data.AST (AST, Node (Node))
import           Data.Blob
import           Data.Duration
import           Data.Term
import           Source.Loc
import qualified Source.Source as Source
import           Source.Span

import qualified TreeSitter.Language as TS
import qualified TreeSitter.Node as TS
import qualified TreeSitter.Parser as TS
import qualified TreeSitter.Tree as TS

-- | Parse 'Source' with the given 'TS.Language' and return its AST.
-- Returns Nothing if the operation timed out.
parseToAST :: ( Bounded grammar
              , Carrier sig m
              , Enum grammar
              , Member Trace sig
              , MonadIO m
              )
           => Duration
           -> Ptr TS.Language
           -> Blob
           -> m (Maybe (AST [] grammar))
parseToAST parseTimeout language b@Blob{..} = do
  result <- liftIO . TS.withParser language $ \ parser -> do
    let timeoutMicros = fromIntegral $ toMicroseconds parseTimeout
    TS.ts_parser_set_timeout_micros parser timeoutMicros
    TS.ts_parser_halt_on_error parser (CBool 1)
    compatible <- TS.ts_parser_set_language parser language
    if compatible then
      TS.withParseTree parser (Source.bytes blobSource) $ \ treePtr ->
        TS.withRootNode treePtr $ \ rootPtr ->
          if treePtr == nullPtr then
            pure (Left "tree-sitter: null root node")
          else do
            TS.ts_tree_root_node_p treePtr rootPtr
            ptr <- peek rootPtr
            Right <$> anaM toAST ptr
    else
      pure (Left "tree-sitter: incompatible versions")
  case result of
    Left  err -> Nothing  <$ trace err <* trace ("tree-sitter: parsing failed " <> blobPath b)
    Right ast -> Just ast <$ trace ("tree-sitter: parsing succeeded " <> blobPath b)

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
