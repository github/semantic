{-# LANGUAGE GADTs, DuplicateRecordFields, RankNTypes, UndecidableInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Arguments where

import Data.Maybe
import Data.String
import Language
import Prologue
import qualified Semantic.Task as Task
import Text.Show

data DiffMode = DiffStdin | DiffCommits String String [(FilePath, Maybe Language)] | DiffPaths (FilePath, Maybe Language) (FilePath, Maybe Language)
  deriving Show

data DiffArguments where
  DiffArguments :: (Monoid output, StringConv output ByteString) =>
    { diffRenderer :: Task.DiffRenderer output
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
patchDiff = DiffArguments Task.PatchDiffRenderer

jsonDiff :: DiffArguments'
jsonDiff = DiffArguments Task.JSONDiffRenderer

sExpressionDiff :: DiffArguments'
sExpressionDiff = DiffArguments Task.SExpressionDiffRenderer

tocDiff :: DiffArguments'
tocDiff = DiffArguments Task.ToCDiffRenderer


data ParseMode = ParseStdin | ParseCommit String [(FilePath, Maybe Language)] | ParsePaths [(FilePath, Maybe Language)]
  deriving Show

data ParseArguments where
  ParseArguments :: (Monoid output, StringConv output ByteString) =>
    { parseTreeRenderer :: Task.TermRenderer output
    , termDecorator :: Task.NamedDecorator
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
sExpressionParseTree = ParseArguments Task.SExpressionTermRenderer Task.IdentityDecorator

jsonParseTree :: ParseArguments'
jsonParseTree = ParseArguments Task.JSONTermRenderer Task.IdentifierDecorator

data ProgramMode = Parse ParseArguments | Diff DiffArguments
  deriving Show

data Arguments = Arguments
  { programMode :: ProgramMode
  , outputFilePath :: Maybe FilePath
  } deriving Show
