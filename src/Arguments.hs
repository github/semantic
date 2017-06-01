{-# LANGUAGE GADTs, DuplicateRecordFields, RankNTypes, StandaloneDeriving, UndecidableInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Arguments where

import Data.Maybe
import Data.String
import Language
import Prologue
import Renderer

data DiffMode = DiffStdin | DiffCommits String String [(FilePath, Maybe Language)] | DiffPaths (FilePath, Maybe Language) (FilePath, Maybe Language)
  deriving Show

data DiffArguments where
  DiffArguments :: (Monoid output, StringConv output ByteString) =>
    { diffRenderer :: DiffRenderer output
    , diffMode :: DiffMode
    , gitDir :: FilePath
    , alternateObjectDirs :: [FilePath]
    } -> DiffArguments

deriving instance Show DiffArguments

type DiffArguments' = DiffMode -> FilePath -> [FilePath] -> DiffArguments

patchDiff :: DiffArguments'
patchDiff = DiffArguments PatchDiffRenderer

jsonDiff :: DiffArguments'
jsonDiff = DiffArguments JSONDiffRenderer

sExpressionDiff :: DiffArguments'
sExpressionDiff = DiffArguments SExpressionDiffRenderer

tocDiff :: DiffArguments'
tocDiff = DiffArguments ToCDiffRenderer


data ParseMode = ParseStdin | ParseCommit String [(FilePath, Maybe Language)] | ParsePaths [(FilePath, Maybe Language)]
  deriving Show

data ParseArguments where
  ParseArguments :: (Monoid output, StringConv output ByteString) =>
    { parseTreeRenderer :: TermRenderer output
    , parseMode :: ParseMode
    , gitDir :: FilePath
    , alternateObjectDirs :: [FilePath]
    } -> ParseArguments

deriving instance Show ParseArguments

type ParseArguments' = ParseMode -> FilePath -> [FilePath] -> ParseArguments

sExpressionParseTree :: ParseArguments'
sExpressionParseTree = ParseArguments SExpressionTermRenderer

jsonParseTree :: ParseArguments'
jsonParseTree = ParseArguments JSONTermRenderer

data ProgramMode = Parse ParseArguments | Diff DiffArguments
  deriving Show

data Arguments = Arguments
  { programMode :: ProgramMode
  , outputFilePath :: Maybe FilePath
  } deriving Show
