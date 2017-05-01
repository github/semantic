{-# LANGUAGE GADTs, DuplicateRecordFields, RankNTypes, StandaloneDeriving, UndecidableInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Arguments where

import Data.Maybe
import Prelude
import Prologue
import Renderer
import Renderer.SExpression
import Info


data DiffMode = DiffCommits String String [FilePath] | DiffPaths FilePath FilePath
  deriving Show

data DiffArguments where
  DiffArguments :: (Monoid output, StringConv output ByteString) =>
    { diffRenderer :: DiffRenderer DefaultFields output
    , diffMode :: DiffMode
    , gitDir :: FilePath
    , alternateObjectDirs :: [FilePath]
    } -> DiffArguments

deriving instance Show DiffArguments

patchDiff :: DiffMode -> FilePath -> [FilePath] -> DiffArguments
patchDiff = DiffArguments PatchRenderer

splitDiff :: DiffMode -> FilePath -> [FilePath] -> DiffArguments
splitDiff = DiffArguments SplitRenderer

jsonDiff :: DiffMode -> FilePath -> [FilePath] -> DiffArguments
jsonDiff = DiffArguments JSONDiffRenderer

summaryDiff :: DiffMode -> FilePath -> [FilePath] -> DiffArguments
summaryDiff = DiffArguments SummaryRenderer

sExpressionDiff :: DiffMode -> FilePath -> [FilePath] -> DiffArguments
sExpressionDiff = DiffArguments (SExpressionDiffRenderer TreeOnly)

tocDiff :: DiffMode -> FilePath -> [FilePath] -> DiffArguments
tocDiff = DiffArguments ToCRenderer


data ParseMode = ParseCommit String [FilePath] | ParsePaths [FilePath]
  deriving Show

data ParseArguments where
  ParseArguments :: (Monoid output, StringConv output ByteString) =>
    { parseTreeRenderer :: ParseTreeRenderer DefaultFields output
    , parseMode :: ParseMode
    , gitDir :: FilePath
    , alternateObjectDirs :: [FilePath]
    } -> ParseArguments

deriving instance Show ParseArguments

sExpressionParseTree :: ParseMode -> FilePath -> [FilePath] -> ParseArguments
sExpressionParseTree = ParseArguments (SExpressionParseTreeRenderer TreeOnly)

jsonParseTree :: ParseMode -> FilePath -> [FilePath] -> ParseArguments
jsonParseTree = ParseArguments JSONParseTreeRenderer

data ProgramMode = Parse ParseArguments | Diff DiffArguments
  deriving Show

data Arguments = Arguments
  { programMode :: ProgramMode
  , outputFilePath :: Maybe FilePath
  } deriving Show
