{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Parsing.TreeSitter
( TSParseException (..)
, Duration(..)
, parseToAST
, parseToPreciseAST
) where

import Control.Carrier.Reader
import Control.Exception as Exc
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Foldable
import Foreign
import GHC.Generics

import           Data.AST (AST, Node (Node))
import           Data.Blob
import           Data.Duration
import           Data.Maybe.Exts
import           Data.Term
import           Source.Loc
import qualified Source.Source as Source
import           Source.Span
import qualified System.Timeout as System

import qualified TreeSitter.Cursor as TS
import qualified TreeSitter.Language as TS
import qualified TreeSitter.Node as TS
import qualified TreeSitter.Parser as TS
import qualified TreeSitter.Tree as TS
import qualified AST.Unmarshal as TS

data TSParseException
  = ParserTimedOut
  | IncompatibleVersions
  | UnmarshalTimedOut
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
           -> m (Either TSParseException (AST grammar))
parseToAST parseTimeout language blob = runParse parseTimeout language blob (anaM toAST <=< peek)

parseToPreciseAST
  :: ( MonadIO m
     , TS.Unmarshal t
     )
  => Duration
  -> Duration
  -> Ptr TS.Language
  -> Blob
  -> m (Either TSParseException (t Loc))
parseToPreciseAST parseTimeout unmarshalTimeout language blob = runParse parseTimeout language blob $ \ rootPtr ->
  withTimeout $
    TS.withCursor (castPtr rootPtr) $ \ cursor ->
      runReader (TS.UnmarshalState (Source.bytes (blobSource blob)) cursor) (liftIO (peek rootPtr) >>= TS.unmarshalNode)
        `Exc.catch` (Exc.throw . UnmarshalFailure . TS.getUnmarshalError)
  where
    withTimeout :: IO a -> IO a
    withTimeout action = System.timeout (toMicroseconds unmarshalTimeout) action >>= maybeM (Exc.throw UnmarshalTimedOut)

instance Exc.Exception TSParseException where
  displayException = \case
    ParserTimedOut -> "tree-sitter: parser timed out"
    IncompatibleVersions -> "tree-sitter: incompatible versions"
    UnmarshalTimedOut -> "tree-sitter: unmarshal timed out"
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
    compatible <- TS.ts_parser_set_language parser language
    if compatible then
      TS.withParseTree parser (Source.bytes blobSource) $ \ treePtr -> do
        if treePtr == nullPtr then
          Exc.throw ParserTimedOut
        else
          TS.withRootNode treePtr action
    else
      Exc.throw IncompatibleVersions

toAST :: forall grammar . (Bounded grammar, Enum grammar) => TS.Node -> IO (Base (AST grammar) TS.Node)
toAST node@TS.Node{..} = do
  let count = fromIntegral nodeChildCount
  children <- allocaArray count $ \ childNodesPtr -> do
    _ <- with nodeTSNode (`TS.ts_node_copy_child_nodes` childNodesPtr)
    peekArray count childNodesPtr
  pure $! In (Node (toEnum (min (fromIntegral nodeSymbol) (fromEnum (maxBound :: grammar)))) (Loc (nodeRange node) (nodeSpan node))) children

anaM :: (Corecursive t, Monad m, Traversable (Base t)) => (a -> m (Base t a)) -> a -> m t
anaM g = a where a = pure . embed <=< traverse a <=< g


nodeRange :: TS.Node -> Range
nodeRange node = Range (fromIntegral (TS.nodeStartByte node)) (fromIntegral (TS.nodeEndByte node))

nodeSpan :: TS.Node -> Span
nodeSpan node = TS.nodeStartPoint node `seq` TS.nodeEndPoint node `seq` Span (pointPos (TS.nodeStartPoint node)) (pointPos (TS.nodeEndPoint node))
  where pointPos TS.TSPoint{..} = pointRow `seq` pointColumn `seq` Pos (1 + fromIntegral pointRow) (1 + fromIntegral pointColumn)
