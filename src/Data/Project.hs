{-# LANGUAGE DeriveAnyClass, KindSignatures, MultiWayIf #-}

module Data.Project (
  -- * Projects
    ProjectF (..)
  , Project
  , PBProject
  , ProjectException (..)
  , fromPB
  , projectExtensions
  , projectName
  , projectFiles
  , readFile
  -- * Files
  , File (..)
  , file
  ) where

import Prelude hiding (readFile)
import Prologue hiding (throwError)

import           Control.Effect
import           Data.Blob
import           Data.Language
import qualified Data.Text as T
import           Proto3.Suite
import           System.FilePath.Posix

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

-- | This 'Project' type is protobuf-compatible, and corresponds with
-- the @Project@ message declaration present in types.proto.
type PBProject = ProjectF NestedVec UnpackedVec Text

deriving instance Message PBProject
instance Named PBProject where nameOf _ = "Project"

-- | Convert from a packed protobuf representation to a more useful one.
fromPB :: PBProject -> Project
fromPB Project {..} = Project
  { projectRootDir     = T.unpack projectRootDir
  , projectBlobs       = toList projectBlobs
  , projectLanguage    = projectLanguage
  , projectExcludeDirs = T.unpack <$> toList projectExcludeDirs
  }

projectName :: Project -> Text
projectName = T.pack . dropExtensions . takeFileName . projectRootDir

projectExtensions :: Project -> [String]
projectExtensions = extensionsForLanguage . projectLanguage

projectFiles :: Project -> [File]
projectFiles = fmap toFile . projectBlobs

data File = File
  { filePath     :: FilePath
  , fileLanguage :: Language
  } deriving (Eq, Ord, Show)

file :: FilePath -> File
file path = File path (languageForFilePath path)
  where languageForFilePath = languageForType . takeExtension

-- This is kind of a wart; Blob and File should be two views of
-- the same higher-kinded datatype.
toFile :: Blob -> File
toFile (Blob _ p l) = File p l

newtype ProjectException
  = FileNotFound FilePath
    deriving (Show, Eq, Typeable, Exception)

readFile :: (Member (Error SomeException) sig, Applicative m, Carrier sig m)
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
