{-# LANGUAGE DataKinds, GADTs #-}
module Command
( Command
-- Constructors
, readFile
, readBlobPairsFromHandle
, readBlobsFromHandle
-- Evaluation
, runCommand
) where

import qualified Command.Files as Files
import Control.Monad.Free.Freer
import Control.Monad.IO.Class
import Data.Functor.Both
import Data.Functor.Classes
import Prologue hiding (readFile)
import Language
import Source
import Text.Show


-- | High-level commands encapsulating the work done to read blobs from the filesystem.
type Command = Freer CommandF


-- Constructors

-- | Read a file into a SourceBlob.
readFile :: FilePath -> Maybe Language -> Command SourceBlob
readFile path lang = ReadFile path lang `Then` return

-- | Read JSON encoded blob pairs to SourceBlobs.
readBlobPairsFromHandle :: Handle -> Command [Both SourceBlob]
readBlobPairsFromHandle h = ReadBlobPairsFromHandle h `Then` return

-- | Read JSON encoded blobs to SourceBlobs.
readBlobsFromHandle :: Handle -> Command [SourceBlob]
readBlobsFromHandle h = ReadBlobsFromHandle h `Then` return


-- Evaluation

-- | Run the passed command and return its results in IO.
runCommand :: Command a -> IO a
runCommand = iterFreerA $ \ command yield -> case command of
  ReadFile path lang -> Files.readFile path lang >>= yield
  ReadBlobPairsFromHandle h -> Files.readBlobPairsFromHandle h >>= yield
  ReadBlobsFromHandle h -> Files.readBlobsFromHandle h >>= yield
  LiftIO io -> io >>= yield


-- Implementation details

data CommandF f where
  ReadFile :: FilePath -> Maybe Language -> CommandF SourceBlob
  ReadBlobPairsFromHandle :: Handle -> CommandF [Both SourceBlob]
  ReadBlobsFromHandle :: Handle -> CommandF [SourceBlob]
  LiftIO :: IO a -> CommandF a

instance MonadIO Command where
  liftIO io = LiftIO io `Then` return

instance Show1 CommandF where
  liftShowsPrec _ _ d command = case command of
    ReadFile path lang -> showsBinaryWith showsPrec showsPrec "ReadFile" d path lang
    ReadBlobPairsFromHandle h -> showsUnaryWith showsPrec "ReadBlobPairsFromHandle" d h
    ReadBlobsFromHandle h -> showsUnaryWith showsPrec "ReadBlobsFromHandle" d h
    LiftIO _ -> showsUnaryWith (const showChar) "LiftIO" d '_'
