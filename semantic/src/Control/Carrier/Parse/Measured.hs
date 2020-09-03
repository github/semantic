{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | A carrier for 'Parse' effects suitable for use in production.
module Control.Carrier.Parse.Measured
( -- * Parse carrier
  ParseC(..)
  -- * Exceptions
, ParsingTimedOut(..)
  -- * Parse effect
, module Control.Effect.Parse
) where

import           Control.Algebra
import           Control.Effect.Error
import           Control.Effect.Lift
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Effect.Trace
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Blob
import qualified Data.Flag as Flag
import           Parsing.Parser
import           Parsing.TreeSitter
import           Semantic.Config
import           Semantic.Task (TaskSession (..))
import           Semantic.Telemetry

newtype ParseC m a = ParseC { runParse :: m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance ( Has (Error SomeException) sig m
         , Has (Reader TaskSession) sig m
         , Has Telemetry sig m
         , Has Trace sig m
         , Has (Lift IO) sig m
         , MonadIO m
         )
      => Algebra (Parse :+: sig) (ParseC m) where
  alg hdl sig ctx = case sig of
    L (Parse parser blob) -> (<$ ctx) <$> runParser blob parser
    R other               -> ParseC (alg (runParse . hdl) other ctx)

-- | Parse a 'Blob' in 'IO'.
runParser ::
  ( Has (Error SomeException) sig m
  , Has (Reader TaskSession) sig m
  , Has Telemetry sig m
  , Has Trace sig m
  , MonadIO m
  )
  => Blob
  -> Parser term
  -> m term
runParser blob parser = case parser of

  UnmarshalParser language ->
    time "parse.tree_sitter_precise_ast_parse" languageTag $ do
      config <- asks config
      executeParserAction (parseToPreciseAST (configTreeSitterParseTimeout config) (configTreeSitterUnmarshalTimeout config) language blob)
    `catchError` \(SomeException e) -> do
      writeStat (increment "parse.precise_ast_parse_failures" languageTag)
      writeLog Error "precise parsing failed" (("task", "parse") : ("exception", "\"" <> displayException e <> "\"") : languageTag)
      throwError (SomeException e)

  where
    languageTag = [("language" :: String, show (blobLanguage blob))]
    executeParserAction act = do
      -- Test harnesses can specify that parsing must fail, for testing purposes.
      shouldFailFlag <- asks (Flag.toBool FailTestParsing . configFailParsingForTesting . config)
      when shouldFailFlag (throwError (SomeException ParsingTimedOut))
      act >>= either (\e -> trace (displayException e) *> throwError (SomeException e)) pure

data ParsingTimedOut = ParsingTimedOut deriving (Eq, Show)
instance Exception ParsingTimedOut
