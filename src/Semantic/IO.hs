{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DuplicateRecordFields, GADTs, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Semantic.IO
( Destination(..)
, Files
, Handle(..)
, IO.IOMode(..)
, NoLanguageForBlob(..)
, Source(..)
, catchException
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
, rethrowing
, runFiles
, runFilesGuided
, stderr
, stdin
, stdout
, write
) where

import qualified Control.Exception as Exc
import           Control.Monad.Effect
import           Control.Monad.Effect.State
import           Control.Monad.Effect.Exception
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Blob
import           Data.Bool
import qualified Data.Project as Project
import           Data.Project (File (..), ProjectException (..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import           Data.Language
import           Data.Source (fromUTF8)
import           Prelude hiding (readFile)
import           Prologue hiding (MonadError (..), fail)
import           System.Directory (doesDirectoryExist)
import qualified System.Directory.Tree as Tree
import           System.Directory.Tree (AnchoredDirTree(..))
import           System.Exit
import           System.FilePath
import           System.FilePath.Glob
import qualified System.IO as IO

-- | Read a utf8-encoded file to a 'Blob'.
readFile :: forall m. MonadIO m => File -> m (Maybe Blob)
readFile (File "/dev/null" _) = pure Nothing
readFile (File path language) = do
  raw <- liftIO $ B.readFile path
  pure . Just . sourceBlob path language . fromUTF8 $ raw

readFilePair :: forall m. MonadIO m => File -> File -> m BlobPair
readFilePair a b = Join <$> join (maybeThese <$> readFile a <*> readFile b)

maybeThese :: Monad m => Maybe a -> Maybe b -> m (These a b)
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

readBlobFromPath :: MonadIO m => File -> m Blob
readBlobFromPath file = do
  maybeFile <- readFile file
  maybeM (fail ("cannot read '" <> show file <> "', file not found or language not supported.")) maybeFile

readProjectFromPaths :: MonadIO m => Maybe FilePath -> FilePath -> Language -> [FilePath] -> m Project.Concrete
readProjectFromPaths maybeRoot path lang excludeDirs = do
  liftIO $ putStrLn "Starting readProjectFromPath"
  isDir <- isDirectory path
  let (filterFun, entryPoints, rootDir) = if isDir
      then (id, [], fromMaybe path maybeRoot)
      else (filter (/= path), [toFile path], fromMaybe (takeDirectory path) maybeRoot)


  paths <- liftIO $ filterFun <$> findFilesInDir rootDir exts excludeDirs
  blobs <- traverse readBlobFromPath (toFile <$> paths)
  let p = Project.Project rootDir blobs lang (filePath <$> entryPoints) excludeDirs
  liftIO $ putStrLn "Done"
  pure p
  where
    toFile path = File path lang
    exts = extensionsForLanguage lang

-- Recursively find files in a directory.
findFilesInDir :: forall m. MonadIO m => FilePath -> [String] -> [FilePath] -> m [FilePath]
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

noLanguageForBlob :: Member (Exc SomeException) effs => FilePath -> Eff effs a
noLanguageForBlob blobPath = throwError (SomeException (NoLanguageForBlob blobPath))


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

readProject :: Member Files effs => Maybe FilePath -> FilePath -> Language -> [FilePath] -> Eff effs Project.Concrete
readProject rootDir dir excludeDirs = send . ReadProject rootDir dir excludeDirs

findFiles :: Member Files effs => FilePath -> [String] -> [FilePath] -> Eff effs [FilePath]
findFiles dir exts = send . FindFiles dir exts

-- | A task which writes a 'B.Builder' to a 'Handle' or a 'FilePath'.
write :: Member Files effs => Destination -> B.Builder -> Eff effs ()
write dest = send . Write dest

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
data Files out where
  Read        :: Source out -> Files out
  ReadProject :: Maybe FilePath -> FilePath -> Language -> [FilePath] -> Files Project.Concrete
  FindFiles   :: FilePath -> [String] -> [FilePath] -> Files [FilePath]
  Write       :: Destination -> B.Builder -> Files ()

-- | Run a 'Files' effect in 'IO'.
runFiles :: (Member (Exc SomeException) effs, Member IO effs) => Eff (Files ': effs) a -> Eff effs a
runFiles = interpret $ \ files -> case files of
  Read (FromPath path)         -> rethrowing (readBlobFromPath path)
  Read (FromHandle handle)     -> rethrowing (readBlobsFromHandle handle)
  Read (FromPathPair paths)    -> rethrowing (runBothWith readFilePair paths)
  Read (FromPairHandle handle) -> rethrowing (readBlobPairsFromHandle handle)
  ReadProject rootDir dir language excludeDirs -> rethrowing (readProjectFromPaths rootDir dir language excludeDirs)
  FindFiles dir exts excludeDirs -> rethrowing (findFilesInDir dir exts excludeDirs)
  Write (ToPath path)                   builder -> liftIO (IO.withBinaryFile path IO.WriteMode (`B.hPutBuilder` builder))
  Write (ToHandle (WriteHandle handle)) builder -> liftIO (B.hPutBuilder handle builder)

runFilesGuided :: (Member (State Project.Concrete) effs, Member (Exc SomeException) effs) => Eff (Files ': effs) a -> Eff effs a
runFilesGuided = interpret $ \files -> case files of
  Read (FromHandle _)            -> throwError (SomeException HandleNotSupported)
  Read (FromPairHandle _)        -> throwError (SomeException HandleNotSupported)
  Write _ _                      -> throwError (SomeException WritesNotSupported)
  Read (FromPath path)           -> get >>= \p -> Project.readBlobFromPath p path
  Read (FromPathPair paths)      -> get >>= \p -> runBothWith (Project.readBlobPair p) paths
  FindFiles dir exts excludeDirs -> get >>= \p -> pure (Project.findFiles (p { Project.projectExcludeDirs = excludeDirs }) dir exts)
  ReadProject{}                  -> get

-- | Catch exceptions in 'IO' actions embedded in 'Eff', handling them with the passed function.
--
--   Note that while the type allows 'IO' to occur anywhere within the effect list, it must actually occur at the end to be able to run the computation.
catchException :: ( Exc.Exception e
                  , Member IO r
                  )
               => Eff r a
               -> (e -> Eff r a)
               -> Eff r a
catchException m handler = interpose pure (\ m yield -> send (Exc.try m) >>= either handler yield) m

-- | Lift an 'IO' action into 'Eff', catching and rethrowing any exceptions it throws into an 'Exc' effect.
rethrowing :: ( Member (Exc SomeException) r
              , Member IO r
              )
           => IO a
           -> Eff r a
rethrowing m = catchException (liftIO m) (throwError . toException @SomeException)
