{-# LANGUAGE ApplicativeDo #-}
module Semantic.CLI (main) where

import           Control.Effect.Reader
import           Control.Exception as Exc (displayException)
import           Data.Blob
import           Data.Blob.IO
import           Data.Handle
import qualified Data.Language as Language
import           Data.List (intercalate)
import           Data.Project
import qualified Data.Text as T
import qualified Data.Flag as Flag
import           Options.Applicative hiding (style)
import           Prologue
import           Semantic.Api hiding (File)
import qualified Semantic.AST as AST
import           Semantic.Config
import qualified Semantic.Graph as Graph
import qualified Semantic.Task as Task
import qualified Semantic.Git as Git
import           Semantic.Task.Files
import           Semantic.Telemetry
import qualified Semantic.Telemetry.Log as Log
import           Semantic.Version
import           Serializing.Format hiding (Options)
import           System.Exit (die)
import           System.FilePath

import Control.Concurrent (mkWeakThreadId, myThreadId)
import Control.Exception (Exception(..), throwTo)
import Data.Typeable (Typeable)
import System.Posix.Signals
import System.Mem.Weak (deRefWeak)

newtype SignalException = SignalException Signal
  deriving (Show, Typeable)
instance Exception SignalException


main :: IO ()
main = do
  args <- getArgs
  parseByteString getArgs
