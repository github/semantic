{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Parse.Simple
( -- * Parse effect
  module Control.Effect.Parse
  -- * Parse carrier
, ParseC(..)
, runParse
  -- * Exceptions
, ParserCancelled(..)
) where

import qualified Assigning.Assignment as Assignment
import qualified Assigning.Assignment.Deterministic as Deterministic
import           Control.Effect.Error
import           Control.Effect.Carrier
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Effect.Trace
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Blob
import           Data.Typeable
import           Parsing.CMark
import           Parsing.Parser
import           Parsing.TreeSitter
import           Prologue hiding (project)

runParse :: Duration -> ParseC m a -> m a
runParse timeout = runReader timeout . runParseC

newtype ParseC m a = ParseC { runParseC :: ReaderC Duration m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance ( Carrier sig m
         , Member (Error SomeException) sig
         , Member Trace sig
         , MonadIO m
         )
      => Carrier (Parse :+: sig) (ParseC m) where
  eff (L (Parse parser blob k)) = ParseC ask >>= \ timeout -> runParser timeout blob parser >>= k
  eff (R other) = ParseC (send (handleCoercible other))

-- | Parse a 'Blob' in 'IO'.
runParser
  :: ( Carrier sig m
     , Member (Error SomeException) sig
     , Member Trace sig
     , MonadIO m
     )
  => Duration
  -> Blob
  -> Parser term
  -> m term
runParser timeout blob@Blob{..} parser = case parser of
  ASTParser language ->
    parseToAST timeout language blob
      >>= either (trace >=> const (throwError (SomeException ParserTimedOut))) pure

  UnmarshalParser language ->
    parseToPreciseAST timeout language blob
      >>= either (trace >=> const (throwError (SomeException ParserTimedOut))) pure

  AssignmentParser    parser assignment ->
    runParser timeout blob parser >>= either (throwError . toException) pure . Assignment.assign    blobSource assignment
  DeterministicParser parser assignment ->
    runParser timeout blob parser >>= either (throwError . toException) pure . Deterministic.assign blobSource assignment

  MarkdownParser ->
    let term = cmarkParser blobSource
    in length term `seq` pure term
  SomeParser parser -> SomeTerm <$> runParser timeout blob parser

data ParserCancelled = ParserTimedOut
  deriving (Show, Typeable)

instance Exception ParserCancelled
