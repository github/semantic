{-# LANGUAGE ExistentialQuantification, GADTs, LambdaCase, KindSignatures, TypeOperators, UndecidableInstances #-}

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
  ) where

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Error
import           Control.Effect.Sum
import           Control.Exception as Exc
import qualified Data.ByteString.Builder as B
import           Data.Blob
import           Data.Coerce
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
data Files (m :: * -> *) k
  = forall a . Read (Source a)                                (a -> k)
  | ReadProject (Maybe FilePath) FilePath Language [FilePath] (Project -> k)
  | FindFiles FilePath [String] [FilePath]                    ([FilePath] -> k)
  | Write Destination B.Builder                               k

deriving instance Functor (Files m)

instance HFunctor Files where
  hmap _ = coerce

instance Effect Files where
  handle state handler (Read source k) = Read source (handler . (<$ state) . k)
  handle state handler (ReadProject rootDir dir language excludeDirs k) = ReadProject rootDir dir language excludeDirs (handler . (<$ state) . k)
  handle state handler (FindFiles dir exts paths k) = FindFiles dir exts paths (handler . (<$ state) . k)
  handle state handler (Write destination builder k) = Write destination builder (handler (k <$ state))

-- | Run a 'Files' effect in 'IO'.
runFiles :: (Member (Error SomeException) sig, MonadIO m, Carrier sig m) => Eff (FilesC m) a -> m a
runFiles = runFilesC . interpret

newtype FilesC m a = FilesC { runFilesC :: m a }

instance (Member (Error SomeException) sig, MonadIO m, Carrier sig m) => Carrier (Files :+: sig) (FilesC m) where
  ret = FilesC . ret
  eff = FilesC . (alg \/ eff . handleCoercible)
    where alg = \case
            Read (FromPath path) k -> (readBlobFromFile' path `catchIO` (throwError . toException @SomeException)) >>= runFilesC . k
            Read (FromHandle handle) k -> (readBlobsFromHandle handle  `catchIO` (throwError . toException @SomeException)) >>= runFilesC . k
            Read (FromPathPair paths) k -> (runBothWith readFilePair paths `catchIO` (throwError . toException @SomeException)) >>= runFilesC . k
            Read (FromPairHandle handle) k -> (readBlobPairsFromHandle handle `catchIO` (throwError . toException @SomeException)) >>= runFilesC . k
            ReadProject rootDir dir language excludeDirs k -> (readProjectFromPaths rootDir dir language excludeDirs `catchIO` (throwError . toException @SomeException)) >>= runFilesC . k
            FindFiles dir exts excludeDirs k -> (findFilesInDir dir exts excludeDirs `catchIO` (throwError . toException @SomeException)) >>= runFilesC . k
            Write (ToPath path) builder k -> liftIO (IO.withBinaryFile path IO.WriteMode (`B.hPutBuilder` builder)) >> runFilesC k
            Write (ToHandle (WriteHandle handle)) builder k -> liftIO (B.hPutBuilder handle builder) >> runFilesC k


readBlob :: (Member Files sig, Carrier sig m) => File -> m Blob
readBlob file = send (Read (FromPath file) ret)

-- | A task which reads a list of 'Blob's from a 'Handle' or a list of 'FilePath's optionally paired with 'Language's.
readBlobs :: (Member Files sig, Carrier sig m, Applicative m) => Either (Handle 'IO.ReadMode) [File] -> m [Blob]
readBlobs (Left handle) = send (Read (FromHandle handle) ret)
readBlobs (Right paths) = traverse (send . flip Read ret . FromPath) paths

-- | A task which reads a list of pairs of 'Blob's from a 'Handle' or a list of pairs of 'FilePath's optionally paired with 'Language's.
readBlobPairs :: (Member Files sig, Carrier sig m, Applicative m) => Either (Handle 'IO.ReadMode) [Both File] -> m [BlobPair]
readBlobPairs (Left handle) = send (Read (FromPairHandle handle) ret)
readBlobPairs (Right paths) = traverse (send . flip Read ret . FromPathPair) paths

readProject :: (Member Files sig, Carrier sig m) => Maybe FilePath -> FilePath -> Language -> [FilePath] -> m Project
readProject rootDir dir lang excludeDirs = send (ReadProject rootDir dir lang excludeDirs ret)

findFiles :: (Member Files sig, Carrier sig m) => FilePath -> [String] -> [FilePath] -> m [FilePath]
findFiles dir exts paths = send (FindFiles dir exts paths ret)

-- | A task which writes a 'B.Builder' to a 'Handle' or a 'FilePath'.
write :: (Member Files sig, Carrier sig m) => Destination -> B.Builder -> m ()
write dest builder = send (Write dest builder (ret ()))


-- | Generalize 'Exc.catch' to other 'MonadIO' contexts for the handler and result.
catchIO :: ( Exc.Exception exc
           , MonadIO m
           )
        => IO a
        -> (exc -> m a)
        -> m a
catchIO m handler = liftIO (Exc.try m) >>= either handler pure
