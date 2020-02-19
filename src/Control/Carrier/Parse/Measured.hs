{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | A carrier for 'Parse' effects suitable for use in production.
module Control.Carrier.Parse.Measured
( -- * Parse carrier
  ParseC(..)
  -- * Exceptions
, AssignmentTimedOut(..)
  -- * Parse effect
, module Control.Effect.Parse
) where

import qualified Assigning.Assignment as Assignment
import           Control.Algebra
import           Control.Effect.Error
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Effect.Trace
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Blob
import qualified Data.Error as Error
import qualified Data.Flag as Flag
import           Data.Foldable
import qualified Data.Syntax as Syntax
import           Parsing.Parser
import           Parsing.TreeSitter
import           Semantic.Config
import           Semantic.Task (TaskSession (..))
import           Semantic.Telemetry
import           Semantic.Timeout
import           Source.Source (Source)

newtype ParseC m a = ParseC { runParse :: m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance ( Has (Error SomeException) sig m
         , Has (Reader TaskSession) sig m
         , Has Telemetry sig m
         , Has Timeout sig m
         , Has Trace sig m
         , MonadIO m
         )
      => Algebra (Parse :+: sig) (ParseC m) where
  alg (L (Parse parser blob k)) = runParser blob parser >>= k
  alg (R other)                 = ParseC (alg (handleCoercible other))

-- | Parse a 'Blob' in 'IO'.
runParser :: (Has (Error SomeException) sig m, Has (Reader TaskSession) sig m, Has Telemetry sig m, Has Timeout sig m, Has Trace sig m, MonadIO m)
          => Blob
          -> Parser term
          -> m term
runParser blob@Blob{..} parser = case parser of
  ASTParser language ->
    time "parse.tree_sitter_ast_parse" languageTag $ do
      config <- asks config
      executeParserAction (parseToAST (configTreeSitterParseTimeout config) language blob)

  UnmarshalParser language ->
    time "parse.tree_sitter_precise_ast_parse" languageTag $ do
      config <- asks config
      executeParserAction (parseToPreciseAST (configTreeSitterParseTimeout config) (configTreeSitterUnmarshalTimeout config) language blob)

  AssignmentParser    parser assignment -> runAssignment Assignment.assign    parser blob assignment

  where 
    languageTag = [("language" :: String, show (blobLanguage blob))]
    executeParserAction act = do
      -- Test harnesses can specify that parsing must fail, for testing purposes.
      shouldFailFlag <- asks (Flag.toBool FailTestParsing . configFailParsingForTesting . config)
      when shouldFailFlag (throwError (SomeException AssignmentTimedOut))
      act >>= either (\e -> trace (displayException e) *> throwError (SomeException e)) pure

data AssignmentTimedOut = AssignmentTimedOut deriving (Show)

instance Exception AssignmentTimedOut



runAssignment
  :: ( Foldable term
     , Syntax.HasErrors term
     , Has (Error SomeException) sig m
     , Has (Reader TaskSession) sig m
     , Has Telemetry sig m
     , Has Timeout sig m
     , Has Trace sig m
     , MonadIO m
     )
  => (Source -> assignment (term Assignment.Loc) -> ast -> Either (Error.Error String) (term Assignment.Loc))
  -> Parser ast
  -> Blob
  -> assignment (term Assignment.Loc)
  -> m (term Assignment.Loc)
runAssignment assign parser blob@Blob{..} assignment = do
  taskSession <- ask
  let requestID' = ("github_request_id", requestID taskSession)
  let isPublic'  = ("github_is_public", show (isPublic taskSession))
  let logPrintFlag = configLogPrintSource . config $ taskSession
  let blobFields = ("path", if isPublic taskSession || Flag.toBool LogPrintSource logPrintFlag then blobPath blob else "<filtered>")
  let logFields = requestID' : isPublic' : blobFields : languageTag
  let shouldFailForTesting = configFailParsingForTesting $ config taskSession
  let shouldFailOnParsing = optionsFailOnParseError . configOptions $ config taskSession
  let shouldFailOnWarning = optionsFailOnWarning . configOptions $ config taskSession

  ast <- runParser blob parser `catchError` \ (SomeException err) -> do
    writeStat (increment "parse.parse_failures" languageTag)
    writeLog Error "failed parsing" (("task", "parse") : logFields)
    throwError (toException err)

  res <- timeout (configAssignmentTimeout (config taskSession)) . time "parse.assign" languageTag $
    case assign blobSource assignment ast of
      Left err -> do
        writeStat (increment "parse.assign_errors" languageTag)
        logError taskSession Error blob err (("task", "assign") : logFields)
        throwError (toException err)
      Right term -> do
        for_ (zip (Syntax.getErrors term) [(0::Integer)..]) $ \ (err, i) -> case Error.errorActual err of
          Just "ParseError" -> do
            when (i == 0) $ writeStat (increment "parse.parse_errors" languageTag)
            logError taskSession Warning blob err (("task", "parse") : logFields)
            when (Flag.toBool FailOnParseError shouldFailOnParsing) (throwError (toException err))
          _ -> do
            when (i == 0) $ writeStat (increment "parse.assign_warnings" languageTag)
            logError taskSession Warning blob err (("task", "assign") : logFields)
            when (Flag.toBool FailOnWarning shouldFailOnWarning) (throwError (toException err))
        term <$ writeStat (count "parse.nodes" (length term) languageTag)
  case res of
    Just r | not (Flag.toBool FailTestParsing shouldFailForTesting) -> pure r
    _ -> do
      writeStat (increment "assign.assign_timeouts" languageTag)
      writeLog Error "assignment timeout" (("task", "assign") : logFields)
      throwError (SomeException AssignmentTimedOut)
  where languageTag = [("language", show (blobLanguage blob))]


-- | Log an 'Error.Error' at the specified 'Level'.
logError :: Has Telemetry sig m
         => TaskSession
         -> Level
         -> Blob
         -> Error.Error String
         -> [(String, String)]
         -> m ()
logError TaskSession{..} level blob err =
  let shouldLogSource = configLogPrintSource config
      shouldColorize = Flag.switch IsTerminal Error.Colourize $ configIsTerminal config
  in writeLog level (Error.formatError shouldLogSource shouldColorize blob err)
