{-# LANGUAGE DeriveAnyClass, KindSignatures, MultiWayIf #-}

module Data.Project
  ( Project (..)
  , ProjectException (..)
  , projectExtensions
  , projectName
  , projectFiles
  , readFile
  , readProjectFromPaths
  ) where

import Prelude hiding (readFile)
import Prologue

import           Control.Effect
import           Control.Effect.Error
import           Data.Blob
import           Data.Blob.IO
import           Data.Language
import qualified Data.Text as T
import           System.FilePath.Posix
import           Semantic.IO

-- | A 'Project' contains all the information that semantic needs
-- to execute an analysis, diffing, or graphing pass.
data Project = Project
  { projectRootDir     :: FilePath
  , projectBlobs       :: [Blob]
  , projectLanguage    :: Language
  , projectExcludeDirs :: [FilePath]
  } deriving (Eq, Show, Generic)

projectName :: Project -> Text
projectName = T.pack . dropExtensions . takeFileName . projectRootDir

projectExtensions :: Project -> [String]
projectExtensions = extensionsForLanguage . projectLanguage

projectFiles :: Project -> [File]
projectFiles = fmap blobFile . projectBlobs

newtype ProjectException
  = FileNotFound FilePath
    deriving (Show, Eq, Typeable, Exception)

readFile :: (Member (Error SomeException) sig, Carrier sig m)
         => Project
         -> File
         -> m (Maybe Blob)
readFile Project{..} f =
  let p         = filePath f
      candidate = find (\b -> blobPath b == p) projectBlobs
  in if
    | p == "/dev/null"  -> pure Nothing
    | isJust candidate  -> pure candidate
    | otherwise         -> throwError (SomeException (FileNotFound p))

readProjectFromPaths :: MonadIO m => Maybe FilePath -> FilePath -> Language -> [FilePath] -> m Project
readProjectFromPaths maybeRoot path lang excludeDirs = do
  isDir <- isDirectory path
  let rootDir = if isDir
      then fromMaybe path maybeRoot
      else fromMaybe (takeDirectory path) maybeRoot

  paths <- liftIO $ findFilesInDir rootDir exts excludeDirs
  blobs <- liftIO $ traverse (readBlobFromFile' . toFile) paths
  pure $ Project rootDir blobs lang excludeDirs
  where
    toFile path = File path lang
    exts = extensionsForLanguage lang
