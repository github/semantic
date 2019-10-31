{-# LANGUAGE RecordWildCards #-}
module Semantic.Telemetry.Error
  ( ErrorLogger
  , ErrorReport (..)
  , ErrorReporter
  , nullErrorReporter
  ) where

import           Control.Exception

data ErrorReport
  = ErrorReport
  { errorReportException :: SomeException
  , errorReportContext   :: [(String, String)]
  } deriving (Show)

-- | Function to log if there are errors reporting an error.
type ErrorLogger = String -> [(String, String)] -> IO ()

type ErrorReporter = ErrorReport -> IO ()

-- | Doesn't send error reports anywhere.  Useful in tests or for basic command-line usage.
nullErrorReporter :: ErrorLogger -> IO ErrorReporter
nullErrorReporter logger = pure reportError
  where
    reportError ErrorReport{..} = let
      msg = takeWhile (/= '\n') (displayException errorReportException)
      in logger msg errorReportContext
