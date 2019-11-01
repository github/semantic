{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecordWildCards, TypeOperators, UndecidableInstances #-}
-- | A carrier for 'Parse' effects suitable for use in the repl, tests, etc.
module Control.Carrier.Parse.Simple
( -- * Parse effect
  module Control.Effect.Parse
  -- * Parse carrier
, ParseC(..)
, runParse
  -- * Exceptions
, ParseFailure(..)
) where

import qualified Assigning.Assignment as Assignment
import           Control.Effect.Error
import           Control.Effect.Carrier
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Blob
import           Parsing.CMark
import           Parsing.Parser
import           Parsing.TreeSitter

runParse :: Duration -> ParseC m a -> m a
runParse timeout = runReader timeout . runParseC

newtype ParseC m a = ParseC { runParseC :: ReaderC Duration m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance ( Carrier sig m
         , Member (Error SomeException) sig
         , MonadIO m
         )
      => Carrier (Parse :+: sig) (ParseC m) where
  eff (L (Parse parser blob k)) = ParseC ask >>= \ timeout -> runParser timeout blob parser >>= k
  eff (R other) = ParseC (send (handleCoercible other))

-- | Parse a 'Blob' in 'IO'.
runParser
  :: ( Carrier sig m
     , Member (Error SomeException) sig
     , MonadIO m
     )
  => Duration
  -> Blob
  -> Parser term
  -> m term
runParser timeout blob@Blob{..} parser = case parser of
  ASTParser language ->
    parseToAST timeout language blob
      >>= either (throwError . SomeException) pure

  UnmarshalParser language ->
    parseToPreciseAST timeout language blob
      >>= either (throwError . SomeException) pure

  AssignmentParser    parser assignment ->
    runParser timeout blob parser >>= either (throwError . toException) pure . Assignment.assign    blobSource assignment

  MarkdownParser ->
    let term = cmarkParser blobSource
    in length term `seq` pure term

newtype ParseFailure = ParseFailure String
  deriving (Show)

instance Exception ParseFailure
