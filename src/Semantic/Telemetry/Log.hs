{-# LANGUAGE RecordWildCards #-}
module Semantic.Telemetry.Log
  ( Level (..)
  , LogOptions (..)
  , Message (..)
  , LogFormatter
  , logfmtFormatter
  , terminalFormatter
  , writeLogMessage
  ) where

import           Data.Error (Colourize (..), withSGRCode)
import           Data.Flag as Flag
import           Data.List (intersperse)
import qualified Data.Time.Format as Time
import qualified Data.Time.LocalTime as LocalTime
import           Prologue
import           System.Console.ANSI
import           System.IO
import           Text.Printf

-- | A log message at a specific level.
data Message = Message Level String [(String, String)] LocalTime.ZonedTime
  deriving (Show)

-- | A formatter function for crafting log messages.
type LogFormatter = LogOptions -> Message -> String

-- | Logging level
data Level
  = Error
  | Warning
  | Info
  | Debug
  deriving (Eq, Ord, Show)

-- | Options for controlling logging
data LogOptions = LogOptions
  { logOptionsLevel      :: Maybe Level  -- ^ What level of messages to log. 'Nothing' disabled logging.
  , logOptionsFormatter  :: LogFormatter -- ^ Log formatter to use.
  , logOptionsContext :: [(String, String)]
  }

-- | Write a log a message to stderr.
writeLogMessage :: MonadIO io => LogOptions -> Message -> io ()
writeLogMessage options@LogOptions{..} = liftIO . hPutStr stderr . logOptionsFormatter options

-- | Format log messaging using "logfmt".
--
-- Logfmt is a loosely defined logging format (see https://brandur.org/logfmt)
-- for structured data, which plays very well with indexing tools like Splunk.
--
-- Example:
--    time=2006-01-02T15:04:05Z07:00 msg="this is a message" key=val int=42 key2="val with word" float=33.33
logfmtFormatter :: LogFormatter
logfmtFormatter LogOptions{..} (Message level message pairs time) =
    showPairs
      ( kv "time" (showTime time)
      : kv "msg" (shows message)
      : kv "level" (shows level)
      : (uncurry kv . second shows <$> (pairs <> logOptionsContext)))
  . showChar '\n' $ ""
  where
    kv k v = showString k . showChar '=' . v
    showPairs = foldr (.) id . intersperse (showChar ' ')
    showTime = showString . Time.formatTime Time.defaultTimeLocale "%FT%XZ%z"

-- | Format log messages to a terminal. Suitable for local development.
--
-- Example:
--     [16:52:41]  INFO this is a message     key=val language=Ruby time=0.000098s
terminalFormatter :: LogFormatter
terminalFormatter LogOptions{..} (Message level message pairs time) =
    showChar '[' . showTime time . showString "] "
  . showLevel level . showChar ' '
  . showString (printf "%-20s " message)
  . showPairs (pairs <> logOptionsContext)
  . showChar '\n' $ ""
  where
    colourize = flag Colourize True
    showLevel Error = withSGRCode colourize [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] (showString "ERROR")
    showLevel Warning = withSGRCode colourize [SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity] (showString " WARN")
    showLevel Info = withSGRCode colourize [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity] (showString " INFO")
    showLevel Debug = withSGRCode colourize [SetColor Foreground Vivid White, SetConsoleIntensity BoldIntensity] (showString "DEBUG")
    showPairs pairs = foldr (.) id $ intersperse (showChar ' ') (showPair <$> pairs)
    showPair (k, v) = showString k . showChar '=' . withSGRCode colourize [SetConsoleIntensity BoldIntensity] (showString v)
    showTime = showString . Time.formatTime Time.defaultTimeLocale "%X"
