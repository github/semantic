{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | A carrier for 'Parse' effects suitable for use in the repl, tests, etc.
module Control.Carrier.Parse.Simple
( -- * Parse carrier
  ParseC(..)
, runParse
  -- * Exceptions
, ParseFailure(..)
  -- * Parse effect
, module Control.Effect.Parse
) where

import qualified Assigning.Assignment as Assignment
import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Effect.Error
import           Control.Effect.Parse
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Blob
import           Parsing.Parser
import           Parsing.TreeSitter

runParse :: Duration -> ParseC m a -> m a
runParse timeout (ParseC m) = runReader timeout m

newtype ParseC m a = ParseC (ReaderC Duration m a)
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance ( Has (Error SomeException) sig m
         , MonadIO m
         )
      => Algebra (Parse :+: sig) (ParseC m) where
  alg (L (Parse parser blob k)) = ParseC ask >>= \ timeout -> runParser timeout blob parser >>= k
  alg (R other)                 = ParseC (send (handleCoercible other))

-- | Parse a 'Blob' in 'IO'.
runParser
  :: ( Has (Error SomeException) sig m
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
    parseToPreciseAST timeout timeout language blob
      >>= either (throwError . SomeException) pure

  AssignmentParser    parser assignment ->
    runParser timeout blob parser >>= either (throwError . toException) pure . Assignment.assign    blobSource assignment

newtype ParseFailure = ParseFailure String
  deriving (Show)

instance Exception ParseFailure
