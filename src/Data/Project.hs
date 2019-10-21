module Data.Project
  ( Project (..)
  , projectExtensions
  , projectName
  , projectFiles
  , readProjectFromPaths
  ) where

import Prelude hiding (readFile)
import Prologue

import           Data.Blob
import           Data.Blob.IO
import           Data.Language
import qualified Data.Text as T
import           System.FilePath.Posix
import           Semantic.IO
import qualified System.Path as Path

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

readProjectFromPaths :: MonadIO m
                     => Maybe Path.AbsRelDir
                     -> FilePath
                     -> Language
                     -> [Path.AbsRelDir]
                     -> m Project
readProjectFromPaths maybeRoot path lang excludeDirs = do
  isDir <- isDirectory path
  let rootDir = if isDir
      then fromMaybe path (fmap Path.toString maybeRoot)
      else fromMaybe (takeDirectory path) (fmap Path.toString maybeRoot)

  paths <- liftIO $ findFilesInDir (Path.absRel rootDir) exts excludeDirs
  blobs <- liftIO $ traverse (readBlobFromFile' . toFile) paths
  pure $ Project rootDir blobs lang (fmap Path.toString excludeDirs)
  where
    toFile path = File (Path.toString path) lang
    exts = extensionsForLanguage lang
