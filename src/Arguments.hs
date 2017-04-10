{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Arguments where

import Command
import Data.Maybe
import Prelude


data DiffMode = DiffCommits String String [FilePath] | DiffPaths FilePath FilePath
  deriving Show

data DiffArguments = DiffArguments
  { encodeDiff :: DiffEncoder
  , diffMode :: DiffMode
  , gitDir :: FilePath
  , alternateObjectDirs :: [FilePath] }

data ParseMode = ParseCommit String [FilePath] | ParsePaths [FilePath]
  deriving Show

data ParseArguments = ParseArguments
  { renderParseTree :: ParseTreeRenderer
  , parseMode :: ParseMode
  , debug :: Bool
  , gitDir :: FilePath
  , alternateObjectDirs :: [FilePath] }

data ProgramMode = Parse ParseArguments | Diff DiffArguments
  deriving Show

data Arguments = Arguments
  { programMode :: ProgramMode
  , outputFilePath :: Maybe FilePath
  } deriving Show


instance Show DiffArguments where
  showsPrec d DiffArguments{..} = showParen (d >= 10) $ showString "DiffArguments "
    . showsPrec 10 (encodeDiff []) . showChar ' '
    . showsPrec 10 diffMode . showChar ' '
    . showsPrec 10 gitDir . showChar ' '
    . showsPrec 10 alternateObjectDirs

instance Show ParseArguments where
  showsPrec d ParseArguments{..} = showParen (d >= 10) $ showString "ParseArguments "
    -- . showsPrec 10 (renderParseTree []) . showChar ' '
    . showsPrec 10 parseMode . showChar ' '
    . showsPrec 10 debug . showChar ' '
    . showsPrec 10 gitDir . showChar ' '
    . showsPrec 10 alternateObjectDirs
