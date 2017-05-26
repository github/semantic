{-# LANGUAGE GADTs, DuplicateRecordFields, RankNTypes, UndecidableInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Arguments where

import Data.Functor.Both (Both)
import Data.Maybe
import Data.Record
import Data.String
import Diff
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
    { diffRenderer :: Renderer (Both SourceBlob, Diff (Syntax Text) (Record fields)) output
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
jsonDiff = DiffArguments JSONRenderer (const identifierDecorator)

sExpressionDiff :: DiffArguments'
sExpressionDiff = DiffArguments SExpressionDiffRenderer identityDecorator

tocDiff :: DiffArguments'
tocDiff = DiffArguments ToCRenderer declarationDecorator


data ParseMode = ParseStdin | ParseCommit String [(FilePath, Maybe Language)] | ParsePaths [(FilePath, Maybe Language)]
  deriving Show

data ParseArguments where
  ParseArguments :: (Monoid output, StringConv output ByteString) =>
    { parseTreeRenderer :: Renderer (Identity SourceBlob, Term (Syntax Text) (Record DefaultFields)) output
    , termDecorator :: Source -> Term (Syntax Text) (Record DefaultFields) -> Term (Syntax Text) (Record fields)
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
sExpressionParseTree = ParseArguments SExpressionParseTreeRenderer identityDecorator

jsonParseTree :: ParseArguments'
jsonParseTree = ParseArguments JSONRenderer (const identifierDecorator)

data ProgramMode = Parse ParseArguments | Diff DiffArguments
  deriving Show

data Arguments = Arguments
  { programMode :: ProgramMode
  , outputFilePath :: Maybe FilePath
  } deriving Show
