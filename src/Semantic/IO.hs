{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DuplicateRecordFields, GADTs, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Semantic.IO
( readFile
, readFilePair
, isDirectory
, readBlobPairsFromHandle
, readBlobsFromHandle
, readBlobsFromPaths
, readBlobsFromDir
, languageForFilePath
, NoLanguageForBlob(..)
, readBlob
, readProject
, readProjectEntry
, readBlobs
, readBlobPairs
, writeToOutput
, Files
, runFiles
, rethrowing
) where

import qualified Control.Exception as Exc
import           Control.Monad.Effect hiding (run)
import           Control.Monad.Effect.Exception
import           Control.Monad.Effect.Run
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Blob as Blob
import           Data.Bool
import           Data.File
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Language
import           Data.Source
import           Prelude hiding (readFile)
import           Prologue hiding (MonadError (..), fail)
import           System.Directory (doesDirectoryExist)
import           System.Exit
import           System.FilePath
import           System.FilePath.Glob
import           System.IO (Handle)
import           Text.Read

-- | Read a utf8-encoded file to a 'Blob'.
readFile :: forall m. MonadIO m => File -> m (Maybe Blob.Blob)
readFile (File "/dev/null" _) = pure Nothing
readFile (File path language) = do
  raw <- liftIO (Just <$> B.readFile path)
  pure $ Blob.sourceBlob path language . fromBytes <$> raw

readFilePair :: forall m. MonadIO m => File -> File -> m Blob.BlobPair
readFilePair a b = do
  before <- readFile a
  after <- readFile b
  case (before, after) of
    (Just a, Nothing) -> pure (Join (This a))
    (Nothing, Just b) -> pure (Join (That b))
    (Just a, Just b)  -> pure (Join (These a b))
    _                 -> fail "expected file pair with content on at least one side"

isDirectory :: MonadIO m => FilePath -> m Bool
isDirectory path = liftIO (doesDirectoryExist path)

-- | Return a language based on a FilePath's extension, or Nothing if extension is not found or not supported.
languageForFilePath :: FilePath -> Maybe Language
languageForFilePath = languageForType . takeExtension

-- | Read JSON encoded blob pairs from a handle.
readBlobPairsFromHandle :: MonadIO m => Handle -> m [Blob.BlobPair]
readBlobPairsFromHandle = fmap toBlobPairs . readFromHandle
  where
    toBlobPairs :: BlobDiff -> [Blob.BlobPair]
    toBlobPairs BlobDiff{..} = toBlobPair <$> blobs
    toBlobPair blobs = toBlob <$> blobs

-- | Read JSON encoded blobs from a handle.
readBlobsFromHandle :: MonadIO m => Handle -> m [Blob.Blob]
readBlobsFromHandle = fmap toBlobs . readFromHandle
  where toBlobs BlobParse{..} = fmap toBlob blobs

readBlobFromPath :: MonadIO m => File -> m Blob.Blob
readBlobFromPath file = do
  maybeFile <- readFile file
  maybe (fail ("cannot read '" <> show file <> "', file not found or language not supported.")) pure maybeFile

readBlobsFromPaths :: MonadIO m => [File] -> m [Blob.Blob]
readBlobsFromPaths files = catMaybes <$> traverse readFile files

readProjectFromPaths :: MonadIO m => FilePath -> Language -> m Project
readProjectFromPaths rootDir lang = do
  paths <- liftIO $ fmap fold (globDir (compile . mappend "**/*." <$> exts) rootDir)
  pure $ Project  rootDir (toFile <$> paths) lang
  where
    toFile path = File path (Just lang)
    exts = extensionsForLanguage lang

readProjectEntryFromPath :: MonadIO m => FilePath -> Language -> m Project
readProjectEntryFromPath path lang = do
  paths <- liftIO $ filter (/= path) <$> fmap fold (globDir (compile . mappend "**/*." <$> exts) rootDir)
  pure $ Project rootDir (toFile <$> (path : paths)) lang
  where
    rootDir = takeDirectory path
    toFile path = File path (Just lang)
    exts = extensionsForLanguage lang

readBlobsFromDir :: MonadIO m => FilePath -> m [Blob.Blob]
readBlobsFromDir path = do
  paths <- liftIO (globDir1 (compile "[^vendor]**/*[.rb|.js|.tsx|.go|.py]") path)
  let paths' = catMaybes $ fmap (\p -> File p . Just <$> languageForFilePath p) paths
  blobs <- traverse readFile paths'
  pure (catMaybes blobs)

readFromHandle :: (FromJSON a, MonadIO m) => Handle -> m a
readFromHandle h = do
  input <- liftIO $ BL.hGetContents h
  case eitherDecode input of
    Left e  -> liftIO (die (e <> ". Invalid input on " <> show h <> ", expecting JSON"))
    Right d -> pure d

toBlob :: Blob -> Blob.Blob
toBlob Blob{..} = Blob.sourceBlob path language' (fromText content)
  where language' = case language of
          "" -> languageForFilePath path
          _  -> readMaybe language


newtype BlobDiff = BlobDiff { blobs :: [BlobPair] }
  deriving (Show, Generic, FromJSON)

newtype BlobParse = BlobParse { blobs :: [Blob] }
  deriving (Show, Generic, FromJSON)

type BlobPair = Join These Blob

data Blob = Blob
  { path     :: FilePath
  , content  :: Text
  , language :: String
  }
  deriving (Show, Generic, FromJSON)

instance FromJSON BlobPair where
  parseJSON = withObject "BlobPair" $ \o -> do
    before <- o .:? "before"
    after <- o .:? "after"
    case (before, after) of
      (Just b, Just a)  -> pure $ Join (These b a)
      (Just b, Nothing) -> pure $ Join (This b)
      (Nothing, Just a) -> pure $ Join (That a)
      _                 -> fail "Expected object with 'before' and/or 'after' keys only"


-- | An exception indicating that we’ve tried to diff or parse a blob of unknown language.
newtype NoLanguageForBlob = NoLanguageForBlob FilePath
  deriving (Eq, Exception, Ord, Show, Typeable)

readBlob :: Member Files effs => File -> Eff effs Blob.Blob
readBlob = send . ReadBlob

-- | A task which reads a list of 'Blob's from a 'Handle' or a list of 'FilePath's optionally paired with 'Language's.
readBlobs :: Member Files effs => Either Handle [File] -> Eff effs [Blob.Blob]
readBlobs = send . ReadBlobs

-- | A task which reads a list of pairs of 'Blob's from a 'Handle' or a list of pairs of 'FilePath's optionally paired with 'Language's.
readBlobPairs :: Member Files effs => Either Handle [Both File] -> Eff effs [Blob.BlobPair]
readBlobPairs = send . ReadBlobPairs

readProject :: Member Files effs => FilePath -> Language -> Eff effs Project
readProject dir = send . ReadProject dir

readProjectEntry :: Member Files effs => FilePath -> Language -> Eff effs Project
readProjectEntry file = send . ReadProjectEntry file

-- | A task which writes a 'B.ByteString' to a 'Handle' or a 'FilePath'.
writeToOutput :: Member Files effs => Either Handle FilePath -> B.ByteString -> Eff effs ()
writeToOutput path = send . WriteToOutput path


-- | An effect to read/write 'Blob.Blob's from 'Handle's or 'FilePath's.
data Files out where
  ReadBlob      :: File -> Files Blob.Blob
  ReadBlobs     :: Either Handle [File] -> Files [Blob.Blob]
  ReadBlobPairs :: Either Handle [Both File] -> Files [Blob.BlobPair]
  ReadProject   :: FilePath -> Language -> Files Project
  ReadProjectEntry   :: FilePath -> Language -> Files Project
  WriteToOutput :: Either Handle FilePath -> B.ByteString -> Files ()

-- | Run a 'Files' effect in 'IO'.
runFiles :: Members '[Exc SomeException, IO] effs => Eff (Files ': effs) a -> Eff effs a
runFiles = interpret $ \ files -> case files of
  ReadBlob path -> rethrowing (readBlobFromPath path)
  ReadBlobs (Left handle) -> rethrowing (readBlobsFromHandle handle)
  ReadBlobs (Right paths@[File path _]) -> rethrowing (isDirectory path >>= bool (readBlobsFromPaths paths) (readBlobsFromDir path))
  ReadBlobs (Right paths) -> rethrowing (readBlobsFromPaths paths)
  ReadBlobPairs source -> rethrowing (either readBlobPairsFromHandle (traverse (runBothWith readFilePair)) source)
  ReadProject dir language -> rethrowing (readProjectFromPaths dir language)
  ReadProjectEntry file language -> rethrowing (readProjectEntryFromPath file language)
  WriteToOutput destination contents -> liftIO (either B.hPutStr B.writeFile destination contents)


instance (Members '[Exc SomeException, IO] effects, Run effects result rest) => Run (Files ': effects) result rest where
  run = run . runFiles


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
