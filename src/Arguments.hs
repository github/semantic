{-# LANGUAGE GADTs, DuplicateRecordFields, RankNTypes, StandaloneDeriving, UndecidableInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Arguments where

import Data.Maybe
import Language
import Prelude
import Prologue
import Renderer
import Renderer.SExpression
import Info

data DiffMode = DiffStdin | DiffCommits String String [(FilePath, Maybe Language)] | DiffPaths (FilePath, Maybe Language) (FilePath, Maybe Language)
  deriving Show

data DiffArguments where
  DiffArguments :: (Monoid output, StringConv output ByteString) =>
    { diffRenderer :: DiffRenderer DefaultFields output
    , diffMode :: DiffMode
    , gitDir :: FilePath
    , alternateObjectDirs :: [FilePath]
    } -> DiffArguments

deriving instance Show DiffArguments

type DiffArguments' = DiffMode -> FilePath -> [FilePath] -> DiffArguments

patchDiff :: DiffArguments'
patchDiff = DiffArguments PatchRenderer

jsonDiff :: DiffArguments'
jsonDiff = DiffArguments JSONDiffRenderer

summaryDiff :: DiffArguments'
summaryDiff = DiffArguments SummaryRenderer

sExpressionDiff :: DiffArguments'
sExpressionDiff = DiffArguments (SExpressionDiffRenderer TreeOnly)

tocDiff :: DiffArguments'
tocDiff = DiffArguments ToCRenderer


data ParseMode = ParseCommit String [(FilePath, Maybe Language)] | ParsePaths [(FilePath, Maybe Language)]
  deriving Show

data ParseArguments where
  ParseArguments :: (Monoid output, StringConv output ByteString) =>
    { parseTreeRenderer :: ParseTreeRenderer DefaultFields output
    , parseMode :: ParseMode
    , gitDir :: FilePath
    , alternateObjectDirs :: [FilePath]
    } -> ParseArguments

deriving instance Show ParseArguments

type ParseArguments' = ParseMode -> FilePath -> [FilePath] -> ParseArguments

sExpressionParseTree :: ParseArguments'
sExpressionParseTree = ParseArguments (SExpressionParseTreeRenderer TreeOnly)

jsonParseTree :: ParseArguments'
jsonParseTree = ParseArguments JSONParseTreeRenderer

data ProgramMode = Parse ParseArguments | Diff DiffArguments
  deriving Show

data Arguments = Arguments
  { programMode :: ProgramMode
  , outputFilePath :: Maybe FilePath
  } deriving Show
