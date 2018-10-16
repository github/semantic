{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DuplicateRecordFields, ExistentialQuantification, GADTs, KindSignatures, LambdaCase, TypeOperators, UndecidableInstances #-}
module Semantic.IO
( Destination(..)
, Files
, Handle(..)
, IO.IOMode(..)
, NoLanguageForBlob(..)
, Source(..)
, findFiles
, findFilesInDir
, getHandle
, isDirectory
, languageForFilePath
, noLanguageForBlob
, openFileForReading
, readBlob
, readBlobFromPath
, readBlobPairs
, readBlobPairsFromHandle
, readBlobs
, readBlobsFromDir
, readBlobsFromHandle
, decodeBlobPairs
, decodeBlobs
, readFile
, readFilePair
, readProject
, readProjectFromPaths
, runFiles
, FilesC(..)
, stderr
, stdin
, stdout
, write
) where

import           Control.Effect
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Blob
import           Data.Bool
import           Data.Coerce
import           Data.Project hiding (readFile)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import           Data.Language
import           Data.Source (fromUTF8)
import           Prelude hiding (fail, readFile)
import           Prologue hiding (MonadError (..))
import           System.Directory (doesDirectoryExist)
import qualified System.Directory.Tree as Tree
import           System.Directory.Tree (AnchoredDirTree(..))
import           System.Exit
import           System.FilePath
import           System.FilePath.Glob
import qualified System.IO as IO

-- | Read a utf8-encoded file to a 'Blob'.
readFile :: MonadIO m => File -> m (Maybe Blob)
readFile (File "/dev/null" _) = pure Nothing
readFile (File path language) = do
  raw <- liftIO $ B.readFile path
  pure . Just . sourceBlob path language . fromUTF8 $ raw

readFilePair :: (MonadIO m, MonadFail m) => File -> File -> m BlobPair
readFilePair a b = Join <$> join (maybeThese <$> readFile a <*> readFile b)

maybeThese :: MonadFail m => Maybe a -> Maybe b -> m (These a b)
maybeThese a b = case (a, b) of
  (Just a, Nothing) -> pure (This a)
  (Nothing, Just b) -> pure (That b)
  (Just a, Just b)  -> pure (These a b)
  _                 -> fail "expected file pair with content on at least one side"

newtype Blobs a = Blobs { blobs :: [a] }
  deriving (Generic, FromJSON)

isDirectory :: MonadIO m => FilePath -> m Bool
isDirectory path = liftIO (doesDirectoryExist path)

decodeBlobPairs :: BL.ByteString -> Either String [BlobPair]
decodeBlobPairs = fmap blobs <$> eitherDecode

-- | Read JSON encoded blob pairs from a handle.
readBlobPairsFromHandle :: MonadIO m => Handle 'IO.ReadMode -> m [BlobPair]
readBlobPairsFromHandle = fmap blobs <$> readFromHandle

decodeBlobs :: BL.ByteString -> Either String [Blob]
decodeBlobs = fmap blobs <$> eitherDecode

-- | Read JSON encoded blobs from a handle.
readBlobsFromHandle :: MonadIO m => Handle 'IO.ReadMode -> m [Blob]
readBlobsFromHandle = fmap blobs <$> readFromHandle

readBlobFromPath :: (MonadFail m, MonadIO m) => File -> m Blob
readBlobFromPath file = do
  maybeFile <- readFile file
  maybeM (fail ("cannot read '" <> show file <> "', file not found or language not supported.")) maybeFile

readProjectFromPaths :: MonadIO m => Maybe FilePath -> FilePath -> Language -> [FilePath] -> m Project
readProjectFromPaths maybeRoot path lang excludeDirs = do
  isDir <- isDirectory path
  let rootDir = if isDir
      then fromMaybe path maybeRoot
      else fromMaybe (takeDirectory path) maybeRoot

  paths <- liftIO $ findFilesInDir rootDir exts excludeDirs
  blobs <- liftIO $ traverse (readBlobFromPath . toFile) paths
  pure $ Project rootDir blobs lang excludeDirs
  where
    toFile path = File path lang
    exts = extensionsForLanguage lang

-- Recursively find files in a directory.
findFilesInDir :: MonadIO m => FilePath -> [String] -> [FilePath] -> m [FilePath]
findFilesInDir path exts excludeDirs = do
  _:/dir <- liftIO $ Tree.build path
  pure $ (onlyFiles . Tree.filterDir (withExtensions exts) . Tree.filterDir (notIn excludeDirs)) dir
  where
    -- Build a list of only FilePath's (remove directories and failures)
    onlyFiles (Tree.Dir _ fs)   = concatMap onlyFiles fs
    onlyFiles (Tree.Failed _ _) = []
    onlyFiles (Tree.File _ f)   = [f]

    -- Predicate for Files with one of the extensions in 'exts'.
    withExtensions exts (Tree.File n _)
      | takeExtension n `elem` exts = True
      | otherwise                   = False
    withExtensions _ _              = True

    -- Predicate for contents NOT in a directory
    notIn dirs (Tree.Dir n _)
      | (x:_) <- n, x == '.' = False -- Don't include directories that start with '.'.
      | n `elem` dirs = False
      | otherwise = True
    notIn _ _ = True

readBlobsFromDir :: MonadIO m => FilePath -> m [Blob]
readBlobsFromDir path = do
  paths <- liftIO (globDir1 (compile "[^vendor]**/*[.rb|.js|.tsx|.go|.py]") path)
  let paths' = fmap (\p -> File p (languageForFilePath p)) paths
  blobs <- traverse readFile paths'
  pure (catMaybes blobs)

readFromHandle :: (FromJSON a, MonadIO m) => Handle 'IO.ReadMode -> m a
readFromHandle (ReadHandle h) = do
  input <- liftIO $ BL.hGetContents h
  case eitherDecode input of
    Left e  -> liftIO (die (e <> ". Invalid input on " <> show h <> ", expecting JSON"))
    Right d -> pure d


-- | An exception indicating that we’ve tried to diff or parse a blob of unknown language.
newtype NoLanguageForBlob = NoLanguageForBlob FilePath
  deriving (Eq, Exception, Ord, Show, Typeable)

noLanguageForBlob :: (Member (Error SomeException) sig, Carrier sig m) => FilePath -> m a
noLanguageForBlob blobPath = throwError (SomeException (NoLanguageForBlob blobPath))


readBlob :: (Member Files sig, Carrier sig m) => File -> m Blob
readBlob file = send (Read (FromPath file) gen)

-- | A task which reads a list of 'Blob's from a 'Handle' or a list of 'FilePath's optionally paired with 'Language's.
readBlobs :: (Member Files sig, Carrier sig m, Applicative m) => Either (Handle 'IO.ReadMode) [File] -> m [Blob]
readBlobs (Left handle) = send (Read (FromHandle handle) gen)
readBlobs (Right paths) = traverse (send . flip Read gen . FromPath) paths

-- | A task which reads a list of pairs of 'Blob's from a 'Handle' or a list of pairs of 'FilePath's optionally paired with 'Language's.
readBlobPairs :: (Member Files sig, Carrier sig m, Applicative m) => Either (Handle 'IO.ReadMode) [Both File] -> m [BlobPair]
readBlobPairs (Left handle) = send (Read (FromPairHandle handle) gen)
readBlobPairs (Right paths) = traverse (send . flip Read gen . FromPathPair) paths

readProject :: (Member Files sig, Carrier sig m) => Maybe FilePath -> FilePath -> Language -> [FilePath] -> m Project
readProject rootDir dir lang excludeDirs = send (ReadProject rootDir dir lang excludeDirs gen)

findFiles :: (Member Files sig, Carrier sig m) => FilePath -> [String] -> [FilePath] -> m [FilePath]
findFiles dir exts paths = send (FindFiles dir exts paths gen)

-- | A task which writes a 'B.Builder' to a 'Handle' or a 'FilePath'.
write :: (Member Files sig, Carrier sig m) => Destination -> B.Builder -> m ()
write dest builder = send (Write dest builder (gen ()))

data Handle mode where
  ReadHandle  :: IO.Handle -> Handle 'IO.ReadMode
  WriteHandle :: IO.Handle -> Handle 'IO.WriteMode

deriving instance Eq   (Handle mode)
deriving instance Show (Handle mode)

getHandle :: Handle mode -> IO.Handle
getHandle (ReadHandle  handle) = handle
getHandle (WriteHandle handle) = handle

stdin :: Handle 'IO.ReadMode
stdin = ReadHandle IO.stdin

stdout :: Handle 'IO.WriteMode
stdout = WriteHandle IO.stdout

stderr :: Handle 'IO.WriteMode
stderr = WriteHandle IO.stderr

openFileForReading :: FilePath -> IO (Handle 'IO.ReadMode)
openFileForReading path = ReadHandle <$> IO.openFile path IO.ReadMode

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
  gen = FilesC . gen
  alg = FilesC . (algF \/ (alg . handlePure runFilesC))
    where algF = \case
            Read (FromPath path) k -> (readBlobFromPath path `catchIO` (throwError . toException @SomeException)) >>= runFilesC . k
            Read (FromHandle handle) k -> (readBlobsFromHandle handle  `catchIO` (throwError . toException @SomeException)) >>= runFilesC . k
            Read (FromPathPair paths) k -> (runBothWith readFilePair paths `catchIO` (throwError . toException @SomeException)) >>= runFilesC . k
            Read (FromPairHandle handle) k -> (readBlobPairsFromHandle handle `catchIO` (throwError . toException @SomeException)) >>= runFilesC . k
            ReadProject rootDir dir language excludeDirs k -> (readProjectFromPaths rootDir dir language excludeDirs `catchIO` (throwError . toException @SomeException)) >>= runFilesC . k
            FindFiles dir exts excludeDirs k -> (findFilesInDir dir exts excludeDirs `catchIO` (throwError . toException @SomeException)) >>= runFilesC . k
            Write (ToPath path) builder k -> liftIO (IO.withBinaryFile path IO.WriteMode (`B.hPutBuilder` builder)) >> runFilesC k
            Write (ToHandle (WriteHandle handle)) builder k -> liftIO (B.hPutBuilder handle builder) >> runFilesC k
