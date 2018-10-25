{-# LANGUAGE GADTs, KindSignatures, TypeOperators #-}

module Semantic.Task.Files
  ( Files
  , Destination (..)
  , Source (..)
  , runFiles
  , readBlob
  , readBlobs
  , readBlobPairs
  , readProject
  , findFiles
  , write
  , Handle (..)
  ) where

import           Control.Monad.Effect
import           Control.Monad.Effect.Exception
import qualified Data.ByteString.Builder as B
import           Data.Blob
import           Data.File
import           Data.Handle
import           Data.Language
import           Data.Project hiding (readFile)
import           Prelude hiding (readFile)
import           Prologue hiding (MonadError (..), fail)
import           Semantic.IO
import qualified System.IO as IO

data Source blob where
  FromPath       :: File                -> Source Blob
  FromHandle     :: Handle 'IO.ReadMode -> Source [Blob]
  FromPathPair   :: Both File           -> Source BlobPair
  FromPairHandle :: Handle 'IO.ReadMode -> Source [BlobPair]

data Destination = ToPath FilePath | ToHandle (Handle 'IO.WriteMode)

-- | An effect to read/write 'Blob's from 'Handle's or 'FilePath's.
data Files (m :: * -> *) out where
  Read        :: Source out                                           -> Files m out
  ReadProject :: Maybe FilePath -> FilePath -> Language -> [FilePath] -> Files m Project
  FindFiles   :: FilePath -> [String] -> [FilePath]                   -> Files m [FilePath]
  Write       :: Destination -> B.Builder                             -> Files m ()

instance PureEffect Files
instance Effect Files where
  handleState c dist (Request (Read source) k) = Request (Read source) (dist . (<$ c) . k)
  handleState c dist (Request (ReadProject rootDir dir language excludeDirs) k) = Request (ReadProject rootDir dir language excludeDirs) (dist . (<$ c) . k)
  handleState c dist (Request (FindFiles dir exts paths) k) = Request (FindFiles dir exts paths) (dist . (<$ c) . k)
  handleState c dist (Request (Write destination builder) k) = Request (Write destination builder) (dist . (<$ c) . k)

-- | Run a 'Files' effect in 'IO'.
runFiles :: (Member (Exc SomeException) effs, Member (Lift IO) effs, PureEffects effs) => Eff (Files ': effs) a -> Eff effs a
runFiles = interpret $ \ files -> case files of
  Read (FromPath path)         -> rethrowing (readBlobFromFile' path)
  Read (FromHandle handle)     -> rethrowing (readBlobsFromHandle handle)
  Read (FromPathPair paths)    -> rethrowing (runBothWith readFilePair paths)
  Read (FromPairHandle handle) -> rethrowing (readBlobPairsFromHandle handle)
  ReadProject rootDir dir language excludeDirs -> rethrowing (readProjectFromPaths rootDir dir language excludeDirs)
  FindFiles dir exts excludeDirs -> rethrowing (findFilesInDir dir exts excludeDirs)
  Write (ToPath path)                   builder -> liftIO (IO.withBinaryFile path IO.WriteMode (`B.hPutBuilder` builder))
  Write (ToHandle (WriteHandle handle)) builder -> liftIO (B.hPutBuilder handle builder)

readBlob :: Member Files effs => File -> Eff effs Blob
readBlob = send . Read . FromPath

-- | A task which reads a list of 'Blob's from a 'Handle' or a list of 'FilePath's optionally paired with 'Language's.
readBlobs :: Member Files effs => Either (Handle 'IO.ReadMode) [File] -> Eff effs [Blob]
readBlobs (Left handle) = send (Read (FromHandle handle))
readBlobs (Right paths) = traverse (send . Read . FromPath) paths

-- | A task which reads a list of pairs of 'Blob's from a 'Handle' or a list of pairs of 'FilePath's optionally paired with 'Language's.
readBlobPairs :: Member Files effs => Either (Handle 'IO.ReadMode) [Both File] -> Eff effs [BlobPair]
readBlobPairs (Left handle) = send (Read (FromPairHandle handle))
readBlobPairs (Right paths) = traverse (send . Read . FromPathPair) paths

readProject :: Member Files effs => Maybe FilePath -> FilePath -> Language -> [FilePath] -> Eff effs Project
readProject rootDir dir excludeDirs = send . ReadProject rootDir dir excludeDirs

findFiles :: Member Files effs => FilePath -> [String] -> [FilePath] -> Eff effs [FilePath]
findFiles dir exts = send . FindFiles dir exts

-- | A task which writes a 'B.Builder' to a 'Handle' or a 'FilePath'.
write :: Member Files effs => Destination -> B.Builder -> Eff effs ()
write dest = send . Write dest
