{-# LANGUAGE GADTs, DuplicateRecordFields, RankNTypes, StandaloneDeriving, UndecidableInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Arguments where

import Data.Maybe
import Data.Record
import Data.String
import Info
import Language.Ruby.Syntax (decoratorWithAlgebra)
import Prologue
import Renderer
import Renderer.TOC (declarationAlgebra)
import Source
import Syntax
import Term
import Text.Show


data DiffMode = DiffCommits String String [FilePath] | DiffPaths FilePath FilePath
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

patchDiff :: DiffMode -> FilePath -> [FilePath] -> DiffArguments
patchDiff = DiffArguments PatchRenderer (const identity)

jsonDiff :: DiffMode -> FilePath -> [FilePath] -> DiffArguments
jsonDiff = DiffArguments JSONDiffRenderer (const identity)

summaryDiff :: DiffMode -> FilePath -> [FilePath] -> DiffArguments
summaryDiff = DiffArguments SummaryRenderer (const identity)

sExpressionDiff :: DiffMode -> FilePath -> [FilePath] -> DiffArguments
sExpressionDiff = DiffArguments (SExpressionDiffRenderer TreeOnly) (const identity)

tocDiff :: DiffMode -> FilePath -> [FilePath] -> DiffArguments
tocDiff = DiffArguments ToCRenderer (decoratorWithAlgebra . declarationAlgebra)


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
