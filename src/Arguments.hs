{-# LANGUAGE GADTs, DuplicateRecordFields #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Arguments where

import Command
import Data.Maybe
import Prelude
import Renderer


data DiffMode = DiffCommits String String [FilePath] | DiffPaths FilePath FilePath
  deriving Show

data DiffArguments = DiffArguments
  { encodeDiff :: DiffEncoder
  , diffMode :: DiffMode
  , gitDir :: FilePath
  , alternateObjectDirs :: [FilePath]
  } deriving Show

data ParseMode = ParseCommit String [FilePath] | ParsePaths [FilePath]
  deriving Show

data ParseArguments = ParseArguments
  { parseTreeFormat :: DefaultParseTreeRenderer
  , parseMode :: ParseMode
  , debug :: Bool
  , gitDir :: FilePath
  , alternateObjectDirs :: [FilePath]
  } deriving Show

data ProgramMode = Parse ParseArguments | Diff DiffArguments
  deriving Show

data Arguments = Arguments
  { programMode :: ProgramMode
  , outputFilePath :: Maybe FilePath
  } deriving Show
