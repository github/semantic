{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | A carrier for 'Parse' effects suitable for use in the repl, tests, etc.
module Control.Carrier.Parse.Simple
( -- * Parse carrier
  ParseC(ParseC)
, runParse
  -- * Exceptions
, ParseFailure(..)
  -- * Parse effect
, module Control.Effect.Parse
) where

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
runParse timeout = runReader timeout . runParseC

newtype ParseC m a = ParseC { runParseC :: ReaderC Duration m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance ( Has (Error SomeException) sig m
         , MonadIO m
         )
      => Algebra (Parse :+: sig) (ParseC m) where
  alg hdl sig ctx = case sig of
    L (Parse parser blob) -> ParseC ask >>= \ timeout -> (<$ ctx) <$> runParser timeout blob parser
    R other               -> ParseC (alg (runParseC . hdl) (R other) ctx)

-- | Parse a 'Blob' in 'IO'.
runParser
  :: ( Has (Error SomeException) sig m
     , MonadIO m
     )
  => Duration
  -> Blob
  -> Parser term
  -> m term
runParser timeout blob parser = case parser of
  UnmarshalParser language ->
    parseToPreciseAST timeout timeout language blob
      >>= either (throwError . SomeException) pure

newtype ParseFailure = ParseFailure String
  deriving (Show)

instance Exception ParseFailure
