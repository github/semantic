{-# LANGUAGE DataKinds, GADTs #-}
module Command
( Command
-- Constructors
, readFile
, readBlobPairsFromHandle
, readFilesAtSHA
, readFilesAtSHAs
-- Evaluation
, runCommand
) where

import qualified Command.Files as Files
import qualified Command.Git as Git
import Control.Monad.Free.Freer
import Control.Monad.IO.Class
import Data.Functor.Both
import Data.Functor.Classes
import Data.String
import Prologue hiding (readFile)
import Language
import Source
import Text.Show


-- | High-level commands encapsulating the work done to read blobs from the filesystem or Git.
type Command = Freer CommandF


-- Constructors

-- | Read a file into a SourceBlob.
readFile :: FilePath -> Maybe Language -> Command SourceBlob
readFile path lang = ReadFile path lang `Then` return

-- | Read JSON encoded blob pairs to SourceBlobs.
readBlobPairsFromHandle :: Handle -> Command [Both SourceBlob]
readBlobPairsFromHandle h = ReadBlobPairsFromHandle h `Then` return

-- | Read a list of files at the given commit SHA.
readFilesAtSHA :: FilePath -- ^ GIT_DIR
                -> [FilePath] -- ^ GIT_ALTERNATE_OBJECT_DIRECTORIES
                -> [(FilePath, Maybe Language)] -- ^ Specific paths. If empty, return changed paths.
                -> String -- ^ The commit SHA.
                -> Command [SourceBlob] -- ^ A command producing a list of pairs of blobs for the specified files (or all files if none were specified).
readFilesAtSHA gitDir alternates paths sha = ReadFilesAtSHA gitDir alternates paths sha `Then` return

-- | Read a list of files at the states corresponding to the given shas.
readFilesAtSHAs :: FilePath -- ^ GIT_DIR
                -> [FilePath] -- ^ GIT_ALTERNATE_OBJECT_DIRECTORIES
                -> [(FilePath, Maybe Language)] -- ^ Specific paths. If empty, return changed paths.
                -> Both String -- ^ The commit shas for the before & after states.
                -> Command [Both SourceBlob] -- ^ A command producing a list of pairs of blobs for the specified files (or all files if none were specified).
readFilesAtSHAs gitDir alternates paths shas = ReadFilesAtSHAs gitDir alternates paths shas `Then` return


-- Evaluation

-- | Run the passed command and return its results in IO.
runCommand :: Command a -> IO a
runCommand = iterFreerA $ \ command yield -> case command of
  ReadFile path lang -> Files.readFile path lang >>= yield
  ReadBlobPairsFromHandle h -> Files.readBlobPairsFromHandle h >>= yield
  ReadFilesAtSHA gitDir alternates paths sha -> Git.readFilesAtSHA gitDir alternates paths sha >>= yield
  ReadFilesAtSHAs gitDir alternates paths shas -> Git.readFilesAtSHAs gitDir alternates paths shas >>= yield
  LiftIO io -> io >>= yield


-- Implementation details

data CommandF f where
  ReadFile :: FilePath -> Maybe Language -> CommandF SourceBlob
  ReadBlobPairsFromHandle :: Handle -> CommandF [Both SourceBlob]
  ReadFilesAtSHA :: FilePath -> [FilePath] -> [(FilePath, Maybe Language)] -> String -> CommandF [SourceBlob]
  ReadFilesAtSHAs :: FilePath -> [FilePath] -> [(FilePath, Maybe Language)] -> Both String -> CommandF [Both SourceBlob]
  LiftIO :: IO a -> CommandF a

instance MonadIO Command where
  liftIO io = LiftIO io `Then` return

instance Show1 CommandF where
  liftShowsPrec _ _ d command = case command of
    ReadFile path lang -> showsBinaryWith showsPrec showsPrec "ReadFile" d path lang
    ReadBlobPairsFromHandle h -> showsUnaryWith showsPrec "ReadBlobPairsFromHandle" d h
    ReadFilesAtSHA gitDir alternates paths sha -> showsQuaternaryWith showsPrec showsPrec showsPrec showsPrec "ReadFilesAtSHA" d gitDir alternates paths sha
    ReadFilesAtSHAs gitDir alternates paths shas -> showsQuaternaryWith showsPrec showsPrec showsPrec showsPrec "ReadFilesAtSHAs" d gitDir alternates paths shas
    LiftIO _ -> showsUnaryWith (const showChar) "LiftIO" d '_'
    where
      showsQuaternaryWith sp1 sp2 sp3 sp4 name d x y z w = showParen (d > 10) $
        showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z . showChar ' ' . sp4 11 w
