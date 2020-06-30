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
, parseToPreciseAST
) where

import Control.Carrier.Reader
import Control.Exception as Exc
import Control.Monad.IO.Class
import Foreign
import GHC.Generics

import           Data.Blob
import           Data.Duration
import           Data.Maybe.Exts
import           Source.Loc
import qualified Source.Source as Source
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
    UnmarshalFailure s -> "tree-sitter: unmarshal failure: " <> s

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
