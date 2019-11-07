{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, GADTs, LambdaCase, RecordWildCards, ScopedTypeVariables, TypeOperators #-}
module Parsing.TreeSitter
( TSParseException (..)
, Duration(..)
, parseToAST
, parseToPreciseAST
) where

import Prologue

import           Control.Effect.Fail
import           Control.Effect.Lift
import           Control.Effect.Reader
import qualified Control.Exception as Exc
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

import qualified TreeSitter.Cursor as TS
import qualified TreeSitter.Language as TS
import qualified TreeSitter.Node as TS
import qualified TreeSitter.Parser as TS
import qualified TreeSitter.Tree as TS
import qualified TreeSitter.Unmarshal as TS

data TSParseException
  = ParserTimedOut
  | IncompatibleVersions
  | UnmarshalFailure String
    deriving (Eq, Show, Generic)

-- | Parse a 'Blob' with the given 'TS.Language' and return its AST.
-- Returns 'Nothing' if the operation timed out.
parseToAST :: ( Bounded grammar
              , Enum grammar
              , MonadIO m
              )
           => Duration
           -> Ptr TS.Language
           -> Blob
           -> m (Either TSParseException  (AST [] grammar))
parseToAST parseTimeout language blob = runParse parseTimeout language blob (anaM toAST <=< peek)

parseToPreciseAST
  :: ( MonadIO m
     , TS.Unmarshal t
     )
  => Duration
  -> Ptr TS.Language
  -> Blob
  -> m (Either TSParseException (t Loc))
parseToPreciseAST parseTimeout language blob = runParse parseTimeout language blob $ \ rootPtr ->
  TS.withCursor (castPtr rootPtr) $ \ cursor ->
    runM (runFail (runReader cursor (runReader (Source.bytes (blobSource blob)) (TS.peekNode >>= TS.unmarshalNode))))
      >>= either (Exc.throw . UnmarshalFailure) pure

instance Exception TSParseException where
  displayException = \case
    ParserTimedOut -> "tree-sitter: parser timed out"
    IncompatibleVersions -> "tree-sitter: incompatible versions"
    UnmarshalFailure s -> "tree-sitter: unmarshal failure - " <> show s

runParse
  :: MonadIO m
  => Duration
  -> Ptr TS.Language
  -> Blob
  -> (Ptr TS.Node -> IO a)
  -> m (Either TSParseException a)
runParse parseTimeout language Blob{..} action =
  liftIO . Exc.tryJust fromException . TS.withParser language $ \ parser -> do
    let timeoutMicros = fromIntegral $ toMicroseconds parseTimeout
    TS.ts_parser_set_timeout_micros parser timeoutMicros
    TS.ts_parser_halt_on_error parser (CBool 1)
    compatible <- TS.ts_parser_set_language parser language
    if compatible then
      TS.withParseTree parser (Source.bytes blobSource) $ \ treePtr -> do
        if treePtr == nullPtr then
          Exc.throw ParserTimedOut
        else
          TS.withRootNode treePtr action
    else
      Exc.throw IncompatibleVersions

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
