{-# LANGUAGE ExistentialQuantification, GADTs, GeneralizedNewtypeDeriving, KindSignatures, TypeOperators, UndecidableInstances #-}

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
  , FilesC(..)
  , FilesArg(..)
  , Excludes(..)
  ) where

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Catch
import           Control.Effect.Error
import           Control.Effect.Sum
import           Data.Blob
import qualified Data.ByteString.Builder as B
import           Data.File
import           Data.Handle
import           Data.Language
import           Data.Project hiding (readFile)
import           Prelude hiding (readFile)
import           Prologue hiding (catch)
import qualified Semantic.Git as Git
import           Semantic.IO
import qualified System.IO as IO

data Source blob where
  FromPath       :: File                            -> Source Blob
  FromHandle     :: Handle 'IO.ReadMode             -> Source [Blob]
  FromDir        :: FilePath                        -> Source [Blob]
  FromGitRepo    :: FilePath -> Git.OID -> Excludes -> Source [Blob]
  FromPathPair   :: Both File                       -> Source BlobPair
  FromPairHandle :: Handle 'IO.ReadMode             -> Source [BlobPair]

data Destination = ToPath FilePath | ToHandle (Handle 'IO.WriteMode)

data Excludes
  = ExcludePaths [FilePath]
  | ExcludeFromHandle (Handle 'IO.ReadMode)

-- | An effect to read/write 'Blob's from 'Handle's or 'FilePath's.
data Files (m :: * -> *) k
  = forall a . Read (Source a)                                (a -> k)
  | ReadProject (Maybe FilePath) FilePath Language [FilePath] (Project -> k)
  | FindFiles FilePath [String] [FilePath]                    ([FilePath] -> k)
  | Write Destination B.Builder                               k

deriving instance Functor (Files m)
instance HFunctor Files
instance Effect Files

-- | Run a 'Files' effect in 'IO'
runFiles :: FilesC m a -> m a
runFiles = runFilesC

newtype FilesC m a = FilesC { runFilesC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Member (Error SomeException) sig, Member Catch sig, MonadIO m, Carrier sig m) => Carrier (Files :+: sig) (FilesC m) where
  eff (R other) = FilesC (eff (handleCoercible other))
  eff (L op) = case op of
    Read (FromPath path) k                                    -> rethrowing (readBlobFromFile' path) >>= k
    Read (FromHandle handle) k                                -> rethrowing (readBlobsFromHandle handle) >>= k
    Read (FromDir dir) k                                      -> rethrowing (readBlobsFromDir dir) >>= k
    Read (FromGitRepo path sha (ExcludePaths excludePaths)) k -> rethrowing (readBlobsFromGitRepo path sha excludePaths) >>= k
    Read (FromGitRepo path sha (ExcludeFromHandle handle)) k  -> rethrowing (readPathsFromHandle handle >>= readBlobsFromGitRepo path sha) >>= k
    Read (FromPathPair paths) k                               -> rethrowing (runBothWith readFilePair paths) >>= k
    Read (FromPairHandle handle) k                            -> rethrowing (readBlobPairsFromHandle handle) >>= k
    ReadProject rootDir dir language excludeDirs k            -> rethrowing (readProjectFromPaths rootDir dir language excludeDirs) >>= k
    FindFiles dir exts excludeDirs k                          -> rethrowing (findFilesInDir dir exts excludeDirs) >>= k
    Write (ToPath path) builder k                             -> rethrowing (liftIO (IO.withBinaryFile path IO.WriteMode (`B.hPutBuilder` builder))) >> k
    Write (ToHandle (WriteHandle handle)) builder k           -> rethrowing (liftIO (B.hPutBuilder handle builder)) >> k

readBlob :: (Member Files sig, Carrier sig m) => File -> m Blob
readBlob file = send (Read (FromPath file) pure)

-- Various ways to read in files
data FilesArg
  = FilesFromHandle (Handle 'IO.ReadMode)
  | FilesFromPaths [File]
  | FilesFromGitRepo FilePath Git.OID Excludes

-- | A task which reads a list of 'Blob's from a 'Handle' or a list of 'FilePath's optionally paired with 'Language's.
readBlobs :: (Member Files sig, Carrier sig m, MonadIO m) => FilesArg -> m [Blob]
readBlobs (FilesFromHandle handle) = send (Read (FromHandle handle) pure)
readBlobs (FilesFromPaths [path]) = do
  isDir <- isDirectory (filePath path)
  if isDir
    then send (Read (FromDir (filePath path)) pure)
    else pure <$> send (Read (FromPath path) pure)
readBlobs (FilesFromPaths paths) = traverse (send . flip Read pure . FromPath) paths
readBlobs (FilesFromGitRepo path sha excludes) = send (Read (FromGitRepo path sha excludes) pure)

-- | A task which reads a list of pairs of 'Blob's from a 'Handle' or a list of pairs of 'FilePath's optionally paired with 'Language's.
readBlobPairs :: (Member Files sig, Carrier sig m) => Either (Handle 'IO.ReadMode) [Both File] -> m [BlobPair]
readBlobPairs (Left handle) = send (Read (FromPairHandle handle) pure)
readBlobPairs (Right paths) = traverse (send . flip Read pure . FromPathPair) paths

readProject :: (Member Files sig, Carrier sig m) => Maybe FilePath -> FilePath -> Language -> [FilePath] -> m Project
readProject rootDir dir lang excludeDirs = send (ReadProject rootDir dir lang excludeDirs pure)

findFiles :: (Member Files sig, Carrier sig m) => FilePath -> [String] -> [FilePath] -> m [FilePath]
findFiles dir exts paths = send (FindFiles dir exts paths pure)

-- | A task which writes a 'B.Builder' to a 'Handle' or a 'FilePath'.
write :: (Member Files sig, Carrier sig m) => Destination -> B.Builder -> m ()
write dest builder = send (Write dest builder (pure ()))

-- | Catch exceptions thrown in 'IO' and rethrow them in an 'Error' effect.
rethrowing :: (Member Catch sig, Member (Error SomeException) sig, Carrier sig m) => m a -> m a
rethrowing act = act `catchSync` (throwError @SomeException)
