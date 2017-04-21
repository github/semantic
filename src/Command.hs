{-# LANGUAGE DataKinds, GADTs #-}
module Command
( Command
-- Constructors
, readFile
, readFilesAtSHA
, readFilesAtSHAs
, concurrently
-- Evaluation
, runCommand
) where

import qualified Command.Files as Files
import qualified Command.Git as Git
import qualified Control.Concurrent.Async.Pool as Async
import Control.Monad.Free.Freer
import Control.Monad.IO.Class
import Data.Functor.Both
import Data.Functor.Classes
import Data.String
import GHC.Conc (numCapabilities)
import Prologue hiding (concurrently, Concurrently, readFile)
import Source
import Text.Show


-- | High-level commands encapsulating the work done to read blobs from the filesystem or Git.
type Command = Freer CommandF


-- Constructors

-- | Read a file into a SourceBlob.
readFile :: FilePath -> Command SourceBlob
readFile path = ReadFile path `Then` return

-- | Read a list of files at the given commit SHA.
readFilesAtSHA :: FilePath -- ^ GIT_DIR
                -> [FilePath] -- ^ GIT_ALTERNATE_OBJECT_DIRECTORIES
                -> [FilePath] -- ^ Specific paths. If empty, return changed paths.
                -> String -- ^ The commit SHA.
                -> Command [SourceBlob] -- ^ A command producing a list of pairs of blobs for the specified files (or all files if none were specified).
readFilesAtSHA gitDir alternates paths sha = ReadFilesAtSHA gitDir alternates paths sha `Then` return

-- | Read a list of files at the states corresponding to the given shas.
readFilesAtSHAs :: FilePath -- ^ GIT_DIR
                -> [FilePath] -- ^ GIT_ALTERNATE_OBJECT_DIRECTORIES
                -> [FilePath] -- ^ Specific paths. If empty, return changed paths.
                -> Both String -- ^ The commit shas for the before & after states.
                -> Command [Both SourceBlob] -- ^ A command producing a list of pairs of blobs for the specified files (or all files if none were specified).
readFilesAtSHAs gitDir alternates paths shas = ReadFilesAtSHAs gitDir alternates paths shas `Then` return

-- | Run a function over each element of a Traversable concurrently.
concurrently :: Traversable t => t a -> (a -> Command b) -> Command (t b)
concurrently ts f = Concurrently ts f `Then` return


-- Evaluation

-- | Run the passed command and return its results in IO.
runCommand :: Command a -> IO a
runCommand = iterFreerA $ \ command yield -> case command of
  ReadFile path -> Files.readFile path >>= yield
  ReadFilesAtSHA gitDir alternates paths sha -> Git.readFilesAtSHA gitDir alternates paths sha >>= yield
  ReadFilesAtSHAs gitDir alternates paths shas -> Git.readFilesAtSHAs gitDir alternates paths shas >>= yield
  Concurrently ts f -> do
    results <- Async.withTaskGroup numCapabilities $ \ group -> Async.runTask group $ traverse (Async.task . runCommand . f) ts
    yield results
  LiftIO io -> io >>= yield


-- Implementation details

data CommandF f where
  ReadFile :: FilePath -> CommandF SourceBlob
  ReadFilesAtSHA :: FilePath -> [FilePath] -> [FilePath] -> String -> CommandF [SourceBlob]
  ReadFilesAtSHAs :: FilePath -> [FilePath] -> [FilePath] -> Both String -> CommandF [Both SourceBlob]
  Concurrently :: Traversable t => t a -> (a -> Command b) -> CommandF (t b)
  LiftIO :: IO a -> CommandF a

instance MonadIO Command where
  liftIO io = LiftIO io `Then` return

instance Show1 CommandF where
  liftShowsPrec sp sl d command = case command of
    ReadFile path -> showsUnaryWith showsPrec "ReadFile" d path
    ReadFilesAtSHA gitDir alternates paths sha -> showsQuaternaryWith showsPrec showsPrec showsPrec showsPrec "ReadFilesAtSHA" d gitDir alternates paths sha
    ReadFilesAtSHAs gitDir alternates paths shas -> showsQuaternaryWith showsPrec showsPrec showsPrec showsPrec "ReadFilesAtSHAs" d gitDir alternates paths shas
    Concurrently commands f -> showsBinaryWith (liftShowsPrec sp sl) (const showChar) "Concurrently" d (traverse f commands) '_'
    LiftIO _ -> showsUnaryWith (const showChar) "LiftIO" d '_'
    where
      showsQuaternaryWith sp1 sp2 sp3 sp4 name d x y z w = showParen (d > 10) $
        showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z . showChar ' ' . sp4 11 w
