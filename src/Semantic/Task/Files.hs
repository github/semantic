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
  ) where

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Error
import           Control.Effect.Sum
import           Control.Exception as Exc
import           Data.Blob
import qualified Data.ByteString.Builder as B
import           Data.File
import           Data.Handle
import           Data.Language
import           Data.Project hiding (readFile)
import           Prelude hiding (readFile)
import           Prologue
import qualified Semantic.Git as Git
import           Semantic.IO
import           Semantic.Telemetry
import qualified System.IO as IO

data Source blob where
  FromPath       :: File                              -> Source Blob
  FromHandle     :: Handle 'IO.ReadMode               -> Source [Blob]
  FromDir        :: FilePath                          -> Source [Blob]
  FromGitRepo    :: FilePath -> Git.OID -> [FilePath] -> Source [Blob]
  FromPathPair   :: Both File                         -> Source BlobPair
  FromPairHandle :: Handle 'IO.ReadMode               -> Source [BlobPair]

data Destination = ToPath FilePath | ToHandle (Handle 'IO.WriteMode)

-- | An effect to read/write 'Blob's from 'Handle's or 'FilePath's.
data Files (m :: * -> *) k
  = forall a . Read (Source a)                                (a -> k)
  | ReadProject (Maybe FilePath) FilePath Language [FilePath] (Project -> k)
  | FindFiles FilePath [String] [FilePath]                    ([FilePath] -> k)
  | Write Destination B.Builder                               k

deriving instance Functor (Files m)
instance HFunctor Files
instance Effect Files

-- | Run a 'Files' effect in 'IO'.
runFiles :: FilesC m a -> m a
runFiles = runFilesC

newtype FilesC m a = FilesC { runFilesC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Member (Error SomeException) sig, Member Telemetry sig, MonadIO m, Carrier sig m) => Carrier (Files :+: sig) (FilesC m) where
  eff (L op) = case op of
    Read (FromPath path) k -> (readBlobFromFile' path `catchIO` (throwError . toException @SomeException)) >>= k
    Read (FromHandle handle) k -> (readBlobsFromHandle handle  `catchIO` (throwError . toException @SomeException)) >>= k
    Read (FromDir dir) k -> (readBlobsFromDir dir `catchIO` (throwError . toException @SomeException)) >>= k
    Read (FromGitRepo path sha excludePaths) k -> time "task.read_git_repo" mempty (readBlobsFromGitRepo path sha excludePaths `catchIO` (throwError . toException @SomeException)) >>= k
    Read (FromPathPair paths) k -> (runBothWith readFilePair paths `catchIO` (throwError . toException @SomeException)) >>= k
    Read (FromPairHandle handle) k -> (readBlobPairsFromHandle handle `catchIO` (throwError . toException @SomeException)) >>= k
    ReadProject rootDir dir language excludeDirs k -> (readProjectFromPaths rootDir dir language excludeDirs `catchIO` (throwError . toException @SomeException)) >>= k
    FindFiles dir exts excludeDirs k -> (findFilesInDir dir exts excludeDirs `catchIO` (throwError . toException @SomeException)) >>= k
    Write (ToPath path) builder k -> liftIO (IO.withBinaryFile path IO.WriteMode (`B.hPutBuilder` builder)) >> k
    Write (ToHandle (WriteHandle handle)) builder k -> liftIO (B.hPutBuilder handle builder) >> k
  eff (R other) = FilesC (eff (handleCoercible other))

readBlob :: (Member Files sig, Carrier sig m) => File -> m Blob
readBlob file = send (Read (FromPath file) pure)

-- Various ways to read in files
data FilesArg
  = FilesFromHandle (Handle 'IO.ReadMode)
  | FilesFromPaths [File]
  | FilesFromGitRepo FilePath Git.OID [FilePath]

-- | A task which reads a list of 'Blob's from a 'Handle' or a list of 'FilePath's optionally paired with 'Language's.
readBlobs :: (Member Files sig, Carrier sig m, MonadIO m) => FilesArg -> m [Blob]
readBlobs (FilesFromHandle handle) = send (Read (FromHandle handle) pure)
readBlobs (FilesFromPaths [path]) = do
  isDir <- isDirectory (filePath path)
  if isDir
    then send (Read (FromDir (filePath path)) pure)
    else pure <$> send (Read (FromPath path) pure)
readBlobs (FilesFromPaths paths) = traverse (send . flip Read pure . FromPath) paths
readBlobs (FilesFromGitRepo path sha excludePaths) = send (Read (FromGitRepo path sha excludePaths) pure)

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


-- | Generalize 'Exc.catch' to other 'MonadIO' contexts for the handler and result.
catchIO :: ( Exc.Exception exc
           , MonadIO m
           )
        => IO a
        -> (exc -> m a)
        -> m a
catchIO m handler = liftIO (Exc.try m) >>= either handler pure
