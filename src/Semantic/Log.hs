module Semantic.Log where

import Data.String
import Prologue hiding (Location, show)
import qualified Data.Time.Format as Time
import qualified Data.Time.LocalTime as LocalTime
import System.Console.ANSI
import System.IO (hIsTerminalDevice)
import System.Posix.Process
import System.Posix.Types
import Text.Show
import Text.Printf

-- | A log message at a specific level.
data Message = Message Level String [(String, String)] LocalTime.ZonedTime
  deriving (Show)

data Level
  = Error
  | Warning
  | Info
  | Debug
  deriving (Eq, Ord, Show)

-- | Format log messaging using "logfmt".
--
-- Logfmt is a loosely defined logging format (see https://brandur.org/logfmt)
-- for structured data, which plays very well with indexing tools like Splunk.
--
-- Example:
--    time=2006-01-02T15:04:05Z07:00 msg="this is a message" key=val int=42 key2="val with word" float=33.33
logfmtFormatter :: Options -> Message -> String
logfmtFormatter Options{..} (Message level message pairs time) =
    showPairs
      ( kv "time" (showTime time)
      : kv "msg" (shows message)
      : kv "level" (shows level)
      : kv "pid" (shows optionsProcessId)
      : (uncurry kv . second shows <$> pairs)
      <> [ kv "request_id" (shows x) | x <- toList optionsRequestId ] )
  . showChar '\n' $ ""
  where
    kv k v = showString k . showChar '=' . v
    showPairs = foldr (.) identity . intersperse (showChar ' ')
    showTime = showString . Time.formatTime Time.defaultTimeLocale "%FT%XZ%z"

-- | Format log messages to a terminal. Suitable for local development.
--
-- Example:
--     [16:52:41]  INFO this is a message     key=val language=Ruby time=0.000098s
terminalFormatter :: Options -> Message -> String
terminalFormatter Options{..} (Message level message pairs time) =
    showChar '[' . showTime time . showString "] "
  . showLevel level . showChar ' '
  . showString (printf "%-20s" message)
  . showPairs pairs
  . showChar '\n' $ ""
  where
    colourize = optionsIsTerminal && optionsEnableColour
    showLevel Error = withSGRCode colourize [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] (showString "ERROR")
    showLevel Warning = withSGRCode colourize [SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity] (showString " WARN")
    showLevel Info = withSGRCode colourize [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity] (showString " INFO")
    showLevel Debug = withSGRCode colourize [SetColor Foreground Vivid White, SetConsoleIntensity BoldIntensity] (showString "DEBUG")
    showPairs pairs = foldr (.) identity $ intersperse (showChar ' ') (showPair <$> pairs)
    showPair (k, v) = showString k . showChar '=' . withSGRCode colourize [SetConsoleIntensity BoldIntensity] (showString v)
    showTime = showString . Time.formatTime Time.defaultTimeLocale "%X"

-- | Options controlling logging, error handling, &c.
data Options = Options
  { optionsEnableColour :: Bool -- ^ Whether to enable colour formatting for logging (Only works when logging to a terminal that supports ANSI colors).
  , optionsLevel :: Maybe Level -- ^ What level of messages to log. 'Nothing' disabled logging.
  , optionsPrintSource :: Bool -- ^ Whether to print the source reference when logging errors.
  , optionsRequestId :: Maybe String -- ^ Optional request id for tracing across systems.
  , optionsIsTerminal :: Bool -- ^ Whether a terminal is attached (set automaticaly at runtime).
  , optionsFormatter :: Options -> Message -> String -- ^ Log formatter to use (set automaticaly at runtime).
  , optionsProcessId :: CPid -- ^ ProcessID (set automaticaly at runtime).
  }

defaultOptions :: Options
defaultOptions = Options
  { optionsEnableColour = True
  , optionsLevel = Just Warning
  , optionsPrintSource = False
  , optionsRequestId = Nothing
  , optionsIsTerminal = False
  , optionsFormatter = logfmtFormatter
  , optionsProcessId = 0
  }

configureOptionsForHandle :: Handle -> Options -> IO Options
configureOptionsForHandle handle options = do
  pid <- getProcessID
  isTerminal <- hIsTerminalDevice handle
  pure $ options
    { optionsIsTerminal = isTerminal
    , optionsFormatter = if isTerminal then terminalFormatter else logfmtFormatter
    , optionsProcessId = pid
    }

withSGRCode :: Bool -> [SGR] -> ShowS -> ShowS
withSGRCode useColour code content =
  if useColour then
    showString (setSGRCode code)
    . content
    . showString (setSGRCode [])
  else
    content
