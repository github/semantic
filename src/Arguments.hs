{-# LANGUAGE GADTs, DuplicateRecordFields, RankNTypes, UndecidableInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Arguments where

import Data.Maybe
import Data.String
import Language
import Prologue
import Renderer
import Text.Show

data DiffMode = DiffStdin | DiffCommits String String [(FilePath, Maybe Language)] | DiffPaths (FilePath, Maybe Language) (FilePath, Maybe Language)
  deriving Show

data DiffArguments where
  DiffArguments :: (Monoid output, StringConv output ByteString) =>
    { diffRenderer :: DiffRenderer output
    , diffMode :: DiffMode
    , gitDir :: FilePath
    , alternateObjectDirs :: [FilePath]
    } -> DiffArguments

instance Show DiffArguments where
  showsPrec d DiffArguments{..} = showParen (d > 10) $ showString "DiffArguments { " . foldr (.) identity (intersperse (showString ", ") fields) . showString " }"
    where fields = [ showString "diffRenderer " . shows diffRenderer
                   , showString "termDecorator _"
                   , showString "diffMode " . shows diffMode
                   , showString "gitDir " . shows gitDir
                   , showString "alternateObjectDirs " . shows alternateObjectDirs ]

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

instance Show ParseArguments where
  showsPrec d ParseArguments{..} = showParen (d > 10) $ showString "ParseArguments { " . foldr (.) identity (intersperse (showString ", ") fields) . showString " }"
    where fields = [ showString "parseTreeRenderer " . shows parseTreeRenderer
                   , showString "termDecorator _"
                   , showString "parseMode " . shows parseMode
                   , showString "gitDir " . shows gitDir
                   , showString "alternateObjectDirs " . shows alternateObjectDirs ]

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
