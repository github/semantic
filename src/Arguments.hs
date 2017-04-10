{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Arguments where

import Data.Maybe
import Prologue hiding ((<>))
import Prelude
import qualified Renderer as R

data DiffMode = DiffCommits String String [FilePath] | DiffPaths FilePath FilePath
  deriving (Show)

data DiffArguments = DiffArguments
  { diffFormat :: R.Format
  , diffMode :: DiffMode
  , gitDir :: FilePath
  , alternateObjectDirs :: [FilePath] }
  deriving (Show)

data ParseMode = ParseCommit String [FilePath] | ParsePaths [FilePath]
  deriving (Show)

data ParseArguments = ParseArguments
  { parseFormat :: R.ParseFormat
  , parseMode :: ParseMode
  , debug :: Bool
  , gitDir :: FilePath
  , alternateObjectDirs :: [FilePath] }
  deriving (Show)

data ProgramMode = Parse ParseArguments | Diff DiffArguments
  deriving (Show)

data Arguments = Arguments
  { programMode :: ProgramMode
  , outputFilePath :: Maybe FilePath
  } deriving (Show)

-- | Quickly assemble an Arguments data record with defaults.
args :: FilePath -> String -> String -> [String] -> R.Format -> Arguments
args gitDir sha1 sha2 paths format = Arguments
  { programMode = Diff DiffArguments
      { diffFormat = format
      , diffMode = DiffCommits sha1 sha2 paths
      , gitDir = gitDir
      , alternateObjectDirs = []
      }
  , outputFilePath = Nothing
  }
