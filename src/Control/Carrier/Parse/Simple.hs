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
import qualified Data.Error as Error
import           Data.Sum
import           Data.Term
import           Data.Typeable
import           Parsing.CMark
import           Parsing.Parser
import           Parsing.TreeSitter
import           Prologue hiding (project)
import           Source.Source (Source)

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
      >>= maybeM (throwError (SomeException ParserTimedOut))

  UnmarshalParser language ->
    parseToPreciseAST timeout language blob
      >>= maybeM (throwError (SomeException ParserTimedOut))

  AssignmentParser    parser assignment -> runAssignment Assignment.assign    parser timeout blob assignment
  DeterministicParser parser assignment -> runAssignment Deterministic.assign parser timeout blob assignment

  MarkdownParser ->
    let term = cmarkParser blobSource
    in length term `seq` pure term
  SomeParser parser -> SomeTerm <$> runParser timeout blob parser

data ParserCancelled = ParserTimedOut
  deriving (Show, Typeable)

instance Exception ParserCancelled


runAssignment
  :: ( Member (Error SomeException) sig
     , Member Trace sig
     , Carrier sig m
     , MonadIO m
     )
  => (Source -> assignment (Term (Sum syntaxes) Assignment.Loc) -> ast -> Either (Error.Error String) (Term (Sum syntaxes) Assignment.Loc))
  -> Parser ast
  -> Duration
  -> Blob
  -> assignment (Term (Sum syntaxes) Assignment.Loc)
  -> m (Term (Sum syntaxes) Assignment.Loc)
runAssignment assign parser timeout blob@Blob{..} assignment = do
  ast <- runParser timeout blob parser
  either (throwError . toException) pure (assign blobSource assignment ast)
