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

data DiffMode = DiffCommits String String [(FilePath, Maybe Language)] | DiffPaths (FilePath, Maybe Language) (FilePath, Maybe Language)
  deriving Show

data DiffArguments where
  DiffArguments :: (Monoid output, StringConv output ByteString) =>
    { diffRenderer :: DiffRenderer DefaultFields output
    , diffMode :: DiffMode
    , gitDir :: FilePath
    , alternateObjectDirs :: [FilePath]
    } -> DiffArguments

deriving instance Show DiffArguments

type Differ' = DiffMode -> FilePath -> [FilePath] -> DiffArguments

patchDiff :: Differ'
patchDiff = DiffArguments PatchRenderer

jsonDiff :: Differ'
jsonDiff = DiffArguments JSONDiffRenderer

summaryDiff :: Differ'
summaryDiff = DiffArguments SummaryRenderer

sExpressionDiff :: Differ'
sExpressionDiff = DiffArguments (SExpressionDiffRenderer TreeOnly)

tocDiff :: Differ'
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

type Parser' = ParseMode -> FilePath -> [FilePath] -> ParseArguments

sExpressionParseTree :: Parser'
sExpressionParseTree = ParseArguments (SExpressionParseTreeRenderer TreeOnly)

jsonParseTree :: Parser'
jsonParseTree = ParseArguments JSONParseTreeRenderer

data ProgramMode = Parse ParseArguments | Diff DiffArguments
  deriving Show

data Arguments = Arguments
  { programMode :: ProgramMode
  , outputFilePath :: Maybe FilePath
  } deriving Show
