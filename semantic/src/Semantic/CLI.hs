{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Semantic.CLI (main) where

import qualified Analysis.File as File
import qualified Control.Carrier.Parse.Measured as Parse
import           Control.Exception
import qualified Data.Flag as Flag
import           Data.Foldable
import           Data.Handle
import           Data.List (intercalate)
import           Options.Applicative hiding (style)
import           Semantic.Api hiding (File)
import           Semantic.Config
import qualified Semantic.Task as Task
import           Semantic.Task.Files
import           Semantic.Telemetry
import qualified Semantic.Telemetry.Log as Log
import           Semantic.Version
import           Serializing.Format
import qualified Source.Language as Language
import           System.Exit (die)

import Control.Concurrent (mkWeakThreadId, myThreadId)
import Proto.Semantic_JSON ()
import System.Mem.Weak (deRefWeak)
import System.Posix.Signals

newtype SignalException = SignalException Signal
  deriving (Show)
instance Exception SignalException

installSignalHandlers :: IO ()
installSignalHandlers = do
  mainThreadId <- myThreadId
  weakTid <- mkWeakThreadId mainThreadId
  for_ [ sigABRT, sigBUS, sigHUP, sigILL, sigQUIT, sigSEGV,
          sigSYS, sigTERM, sigUSR1, sigUSR2, sigXCPU, sigXFSZ ] $ \sig ->
    installHandler sig (Catch $ sendException weakTid sig) Nothing
  where
    sendException weakTid sig = do
      m <- deRefWeak weakTid
      case m of
        Nothing  -> pure ()
        Just tid -> throwTo tid (toException $ SignalException sig)

main :: IO ()
main = do
  installSignalHandlers
  (options, task) <- customExecParser (prefs showHelpOnEmpty) arguments
  config <- defaultConfig options
  res <- withTelemetry config $ \ (TelemetryQueues logger statter _) ->
    Task.runTask (Task.TaskSession config "-" (optionsLogPathsOnError options) logger statter) (Parse.runParse task)
  either (die . displayException) pure res

-- | A parser for the application's command-line arguments.
--
--   Returns a 'Task' to read the input, run the requested operation, and write the output to the specified output path or stdout.
arguments :: ParserInfo (Options, Parse.ParseC Task.TaskC ())
arguments = info (version <*> helper <*> ((,) <$> optionsParser <*> argumentsParser)) description
  where
    version = infoOption versionString (long "version" <> short 'v' <> help "Output the version of the program")
    versionString = "semantic version " <> buildVersion <> " (" <> buildSHA <> ")"
    description = fullDesc <> header "semantic -- Semantic (syntax-aware) diffs, program analysis toolkit"

optionsParser :: Parser Options
optionsParser = do
  logLevel <- options [ ("error", Just Log.Error) , ("warning", Just Log.Warning) , ("info", Just Log.Info) , ("debug", Just Log.Debug) , ("none", Nothing)]
                      (long "log-level" <> value (Just Log.Warning) <> help "Log messages at or above this level, or disable logging entirely.")
  failOnWarning <- switch (long "fail-on-warning" <> help "Fail on assignment warnings.")
  failOnParseError <- switch (long "fail-on-parse-error" <> help "Fail on tree-sitter parse errors.")
  logPathsOnError <- switch (long "log-paths" <> help "Log source paths on parse and assignment error.")
  pure $ Options logLevel logPathsOnError (Flag.flag FailOnWarning failOnWarning) (Flag.flag FailOnParseError failOnParseError)

argumentsParser :: Parser (Parse.ParseC Task.TaskC ())
argumentsParser = do
  subparser <- hsubparser parseCommand
  output <- ToPath <$> pathOption (long "output" <> short 'o' <> help "Output path, defaults to stdout") <|> pure (ToHandle stdout)
  pure $ subparser >>= Task.write output

parseCommand :: Mod CommandFields (Parse.ParseC Task.TaskC Builder)
parseCommand = command "parse" (info parseArgumentsParser (progDesc "Generate parse trees for path(s)"))
  where
    parseArgumentsParser = do
      renderer
        <-  flag  (parseTermBuilder TermSExpression)
                  (parseTermBuilder TermSExpression)
                  (  long "sexpression"
                  <> help "Output s-expression parse trees (default)")
        <|> flag' (parseSymbolsBuilder JSON)
                  (  long "symbols"
                  <> long "json-symbols"
                  <> help "Output JSON symbol list")
        <|> flag' (parseSymbolsBuilder Proto)
                  (  long "proto-symbols"
                  <> help "Output protobufs symbol list")
        <|> flag' (parseTermBuilder TermJSON)
                  (  long "json"
                  <> help "Output JSON AST dump")
        <|> flag' (parseTermBuilder TermShow)
                  (  long "show"
                  <> help "Output using the Show instance (debug only, format subject to change without notice)")
        <|> flag' (parseTermBuilder TermQuiet)
                  (  long "quiet"
                  <> help "Don't produce output, but show timing stats")
      filesOrStdin <- FilesFromPaths <$> some (argument filePathReader (metavar "FILES..."))
                  <|> pure (FilesFromHandle stdin)
      pure $ Task.readBlobs filesOrStdin >>= renderer

filePathReader :: ReadM (File.File Language.Language)
filePathReader = File.fromPath <$> path

path :: ReadM FilePath
path = eitherReader Right

pathOption :: Mod OptionFields FilePath -> Parser FilePath
pathOption = option path

options :: Eq a => [(String, a)] -> Mod OptionFields a -> Parser a
options options fields = option (optionsReader options) (fields <> showDefaultWith (findOption options) <> metavar (intercalate "|" (fmap fst options)))
  where
    optionsReader options = eitherReader $ \ str -> maybe (Left ("expected one of: " <> intercalate ", " (fmap fst options))) (Right . snd) (find ((== str) . fst) options)
    findOption options value = maybe "" fst (find ((== value) . snd) options)
