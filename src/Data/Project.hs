{-# LANGUAGE DeriveAnyClass, KindSignatures, MultiWayIf #-}

module Data.Project (
  -- * Projects
    ProjectF (..)
  , Project
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
import           Data.File
import           Data.Language
import qualified Data.Text as T
import           System.FilePath.Posix
import           Semantic.IO

-- | A 'ProjectF' contains all the information that semantic needs
-- to execute an analysis, diffing, or graphing pass. It is higher-kinded
-- in terms of the container type for paths and blobs, as well as the
-- path type (this is necessary because protobuf uses different vector
-- representations for @repeated string@ and @repeated Blob@.
-- You probably want to use the 'Project' or 'PB' type aliases.
data ProjectF (blobs :: * -> *) (paths :: * -> *) path = Project
  { projectRootDir     :: path
  , projectBlobs       :: blobs Blob
  , projectLanguage    :: Language
  , projectExcludeDirs :: paths path
  } deriving (Functor, Generic)

deriving instance (Eq path, Eq (blobs Blob), Eq (paths path)) => Eq (ProjectF blobs paths path)
deriving instance (Show path, Show (blobs Blob), Show (paths path)) => Show (ProjectF blobs paths path)

-- | This 'Project' type is the one used during semantic's normal
-- course of diffing, evaluation, and graphing. You probably want to
-- use this one.
type Project = ProjectF [] [] FilePath

projectName :: Project -> Text
projectName = T.pack . dropExtensions . takeFileName . projectRootDir

projectExtensions :: Project -> [String]
projectExtensions = extensionsForLanguage . projectLanguage

projectFiles :: Project -> [File]
projectFiles = fmap toFile . projectBlobs

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
