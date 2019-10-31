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
  } deriving (Eq, Show)

projectName :: Project -> Text
projectName = T.pack . dropExtensions . takeFileName . projectRootDir

projectExtensions :: Project -> [String]
projectExtensions = extensionsForLanguage . projectLanguage

projectFiles :: Project -> [File]
projectFiles = fmap blobFile . projectBlobs

readProjectFromPaths :: MonadIO m
                     => Maybe Path.AbsRelDir -- ^ An optional root directory for the project
                     -> Path.AbsRelFileDir   -- ^ A file or directory to parse. Passing a file path loads all files in that file's parent directory.
                     -> Language
                     -> [Path.AbsRelDir]     -- ^ Directories to exclude.
                     -> m Project
readProjectFromPaths maybeRoot path lang excludeDirs = do
  let rootDir :: Path.AbsRelDir
      rootDir = case maybeRoot >>= Path.fromAbsRel of
        -- If we were provided a root directory, use that.
        Just root -> root
        Nothing   -> case Path.fileFromFileDir path of
          -- If we weren't and the path is a file, drop its file name.
          Just fp -> Path.takeDirectory fp
          -- Otherwise, load from the path.
          Nothing -> Path.dirFromFileDir path

  paths <- liftIO $ findFilesInDir rootDir exts excludeDirs
  blobs <- liftIO $ traverse (readBlobFromFile' . toFile) paths
  pure $ Project (Path.toString rootDir) blobs lang (fmap Path.toString excludeDirs)
  where
    toFile path = File (Path.toString path) lang
    exts = extensionsForLanguage lang
