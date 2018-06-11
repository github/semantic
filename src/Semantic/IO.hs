{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DuplicateRecordFields, GADTs, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Semantic.IO
( module X
, NoLanguageForBlob(..)
, findFilesInDir
, isDirectory
, languageForFilePath
, noLanguageForBlob
, readBlobFromPath
, readBlobPairsFromHandle
, readBlobsFromDir
, readBlobsFromHandle
, decodeBlobPairs
, decodeBlobs
, readFile
, readFilePair
, readProjectFromPaths
) where

import           Control.Monad.Effect
import           Control.Monad.Effect.Exception
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Blob
import           Data.Bool
import qualified Data.Project as Project
import           Data.Project (File (..))
import qualified Data.ByteString as B
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

import           Data.Handle as X

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
