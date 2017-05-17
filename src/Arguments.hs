{-# LANGUAGE GADTs, DuplicateRecordFields, RankNTypes, StandaloneDeriving, UndecidableInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Arguments where

import Data.Maybe
import Data.Record
import Data.String
import Info
import Language
import Prologue
import Renderer
import Source
import Syntax
import Term
import Text.Show

data DiffMode = DiffStdin | DiffCommits String String [(FilePath, Maybe Language)] | DiffPaths (FilePath, Maybe Language) (FilePath, Maybe Language)
  deriving Show

data DiffArguments where
  DiffArguments :: (Monoid output, StringConv output ByteString, HasField fields Category, NFData (Record fields)) =>
    { diffRenderer :: DiffRenderer fields output
    , termDecorator :: Source -> Term (Syntax Text) (Record DefaultFields) -> Term (Syntax Text) (Record fields)
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

-- | The identity decorator, i.e. a decorator which ignores the source and passes terms through unchanged.
identityDecorator :: Source -> Term f a -> Term f a
identityDecorator = const identity

patchDiff :: DiffArguments'
patchDiff = DiffArguments PatchRenderer identityDecorator

jsonDiff :: DiffArguments'
jsonDiff = DiffArguments JSONDiffRenderer identityDecorator

sExpressionDiff :: DiffArguments'
sExpressionDiff = DiffArguments (SExpressionDiffRenderer TreeOnly) identityDecorator

tocDiff :: DiffArguments'
tocDiff = DiffArguments ToCRenderer declarationDecorator


data ParseMode = ParseStdin | ParseCommit String [(FilePath, Maybe Language)] | ParsePaths [(FilePath, Maybe Language)]
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
