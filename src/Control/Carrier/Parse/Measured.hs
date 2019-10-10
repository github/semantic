{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TypeOperators, UndecidableInstances #-}
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
import           Data.Sum
import           Data.Term
import           Data.Typeable
import           Parsing.Parser
import           Parsing.TreeSitter
import           Prologue hiding (project)
import           Semantic.Config
import           Semantic.Task (TaskSession(..))
import           Semantic.Telemetry
import           Semantic.Timeout

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
  eff (L (Parse parser blob k)) = case parser of
    UnmarshalParser language ->
      time "parse.tree_sitter_ast_parse" languageTag $ do
        config <- asks config
        parseToPreciseAST (configTreeSitterParseTimeout config) language blob
          >>= either (\e -> trace (displayException e) *> throwError (SomeException e)) k
    AssignmentParser language assignment -> do
      taskSession <- ask
      let requestID' = ("github_request_id", requestID taskSession)
          isPublic'  = ("github_is_public", show (isPublic taskSession))
          logPrintFlag = configLogPrintSource . config $ taskSession
          blobFields = ("path", if isPublic taskSession || Flag.toBool LogPrintSource logPrintFlag then blobPath blob else "<filtered>")
          logFields = requestID' : isPublic' : blobFields : languageTag
          shouldFailForTesting = configFailParsingForTesting $ config taskSession
          shouldFailOnParsing = optionsFailOnParseError . configOptions $ config taskSession
          shouldFailOnWarning = optionsFailOnWarning . configOptions $ config taskSession

      ast <- time "parse.tree_sitter_ast_parse" languageTag $ do
        config <- asks config
        parseToAST (configTreeSitterParseTimeout config) language blob >>= either
          (\ err -> do
            trace (displayException err)
            writeStat (increment "parse.parse_failures" languageTag)
            writeLog Error "failed parsing" (("task", "parse") : logFields)
            throwError (toException err))
          pure

      res <- timeout (configAssignmentTimeout (config taskSession)) . time "parse.assign" languageTag $
        case Assignment.assign (blobSource blob) assignment ast of
          Left err -> do
            writeStat (increment "parse.assign_errors" languageTag)
            logError taskSession Error blob err (("task", "assign") : logFields)
            throwError (toException err)
          Right term -> do
            for_ (zip (errors term) [(0::Integer)..]) $ \ (err, i) -> case Error.errorActual err of
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
        Just r | not (Flag.toBool FailTestParsing shouldFailForTesting) -> k r
        _ -> do
          writeStat (increment "assign.assign_timeouts" languageTag)
          writeLog Error "assignment timeout" (("task", "assign") : logFields)
          throwError (SomeException AssignmentTimedOut)
    where languageTag = [("language" :: String, show (blobLanguage blob))]
  eff (R other) = ParseC (eff (handleCoercible other))


data ParserCancelled = ParserTimedOut | AssignmentTimedOut
  deriving (Show, Typeable)

instance Exception ParserCancelled


errors :: (Syntax.Error :< fs, Apply Foldable fs, Apply Functor fs) => Term (Sum fs) Assignment.Loc -> [Error.Error String]
errors = cata $ \ (In Assignment.Loc{..} syntax) ->
  maybe (fold syntax) (pure . Syntax.unError span) (project syntax)


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
