{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecordWildCards, TypeOperators, UndecidableInstances #-}
-- | A carrier for 'Parse' effects suitable for use in production.
module Control.Carrier.Parse.Measured
( -- * Parse effect
  module Control.Effect.Parse
  -- * Parse carrier
, ParseC(..)
  -- * Exceptions
, ParserCancelled(..)
) where

import qualified Assigning.Assignment as Assignment
import           Control.Effect.Error
import           Control.Effect.Carrier
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Effect.Trace
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Blob
import qualified Data.Error as Error
import qualified Data.Flag as Flag
import qualified Data.Syntax as Syntax
import           Parsing.CMark
import           Parsing.Parser
import           Parsing.TreeSitter
import           Prologue hiding (project)
import           Semantic.Config
import           Semantic.Task (TaskSession(..))
import           Semantic.Telemetry
import           Semantic.Timeout
import           Source.Source (Source)

newtype ParseC m a = ParseC { runParse :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance ( Carrier sig m
         , Member (Error SomeException) sig
         , Member (Reader TaskSession) sig
         , Member Telemetry sig
         , Member Timeout sig
         , Member Trace sig
         , MonadIO m
         )
      => Carrier (Parse :+: sig) (ParseC m) where
  eff (L (Parse parser blob k)) = runParser blob parser >>= k
  eff (R other) = ParseC (eff (handleCoercible other))

-- | Parse a 'Blob' in 'IO'.
runParser :: (Member (Error SomeException) sig, Member (Reader TaskSession) sig, Member Telemetry sig, Member Timeout sig, Member Trace sig, Carrier sig m, MonadIO m)
          => Blob
          -> Parser term
          -> m term
runParser blob@Blob{..} parser = case parser of
  ASTParser language ->
    time "parse.tree_sitter_ast_parse" languageTag $ do
      config <- asks config
      parseToAST (configTreeSitterParseTimeout config) language blob
        >>= either (\e -> trace (displayException e) *> throwError (SomeException e)) pure

  UnmarshalParser language ->
    time "parse.tree_sitter_ast_parse" languageTag $ do
      config <- asks config
      parseToPreciseAST (configTreeSitterParseTimeout config) language blob
        >>= either (\e -> trace (displayException e) *> throwError (SomeException e)) pure

  AssignmentParser    parser assignment -> runAssignment Assignment.assign    parser blob assignment

  MarkdownParser ->
    time "parse.cmark_parse" languageTag $
      let term = cmarkParser blobSource
      in length term `seq` pure term
  where languageTag = [("language" :: String, show (blobLanguage blob))]

data ParserCancelled = ParserTimedOut | AssignmentTimedOut
  deriving (Show)

instance Exception ParserCancelled


runAssignment
  :: ( Foldable term
     , Syntax.HasErrors term
     , Member (Error SomeException) sig
     , Member (Reader TaskSession) sig
     , Member Telemetry sig
     , Member Timeout sig
     , Member Trace sig
     , Carrier sig m
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
logError :: (Member Telemetry sig, Carrier sig m)
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
