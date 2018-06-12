{-# LANGUAGE DeriveAnyClass, KindSignatures, MultiWayIf #-}

module Data.Project (
  -- * Projects
    Project (..)
  , Concrete
  , PB
  , ProjectException (..)
  , fromPB
  , projectExtensions
  , projectName
  , projectEntryPoints
  , projectFiles
  , readFile
  -- * Files
  , File (..)
  , file
  , )where

import Prelude hiding (readFile)
import Prologue hiding (throwError)

import           Control.Monad.Effect
import           Control.Monad.Effect.Exception
import           Data.Blob
import           Data.Language
import qualified Data.Text as T
import           Debug.Trace
import           Proto3.Suite
import           System.FilePath.Posix

-- | A 'Project' contains all the information that semantic needs
-- to execute an analysis, diffing, or graphing pass. It is higher-kinded
-- in terms of the container type for paths and blobs, as well as the
-- path type (this is necessary because protobuf uses different vector
-- representations for @repeated string@ and @repeated Blob@.
-- You probably want to use the 'Concrete' or 'PB' type aliases.
data Project (blobs :: * -> *) (paths :: * -> *) path = Project
  { projectRootDir     :: path
  , projectBlobs       :: blobs Blob
  , projectLanguage    :: Language
  , projectEntryPaths  :: paths path
  , projectExcludeDirs :: paths path
  } deriving (Functor, Generic, Named)

deriving instance ( MessageField path
                  , MessageField (paths path)
                  , MessageField (blobs Blob)
                  ) => Message (Project blobs paths path)

deriving instance (Eq path, Eq (blobs Blob), Eq (paths path)) => Eq (Project blobs paths path)
deriving instance (Show path, Show (blobs Blob), Show (paths path)) => Show (Project blobs paths path)

-- | This 'Project' type is the one used during semantic's normal
-- course of diffing, evaluation, and graphing. You probably want to
-- use this one.
type Concrete = Project [] [] FilePath

-- | This 'Project' type is protobuf-compatible, and corresponds with
-- the @Project@ message declaration present in types.proto.
type PB = Project NestedVec UnpackedVec Text

-- | Convert from a packed protobuf representatio nto a more useful one.
fromPB :: PB -> Concrete
fromPB Project {..} = Project
  { projectRootDir     = T.unpack projectRootDir
  , projectBlobs       = toList projectBlobs
  , projectLanguage    = projectLanguage
  , projectEntryPaths  = T.unpack <$> toList projectEntryPaths
  , projectExcludeDirs = T.unpack <$> toList projectExcludeDirs
  }

projectName :: Concrete -> Text
projectName = T.pack . dropExtensions . takeFileName . projectRootDir

projectExtensions :: Concrete -> [String]
projectExtensions = extensionsForLanguage . projectLanguage

projectEntryPoints :: Concrete -> [File]
projectEntryPoints (Project {..})= foldr go [] projectBlobs
  where go b acc =
          if blobPath b `elem` projectEntryPaths
          then toFile b : acc
          else acc

projectFiles :: Concrete -> [File]
projectFiles = fmap toFile . projectBlobs where

data File = File
  { filePath     :: FilePath
  , fileLanguage :: Language
  } deriving (Eq, Ord, Show)

file :: FilePath -> File
file path = File path (languageForFilePath path)
  where languageForFilePath = languageForType . takeExtension

-- This is kind of a wart; Blob should really hold a 'File'.
toFile :: Blob -> File
toFile (Blob _ p l) = File p l

data ProjectException
  = FileNotFound FilePath
    deriving (Show, Eq, Typeable, Exception)

readFile :: Member (Exc SomeException) effs
         => Concrete
         -> File
         -> Eff effs (Maybe Blob)
readFile Project{..} f =
  let p         = filePath f
      candidate = find (\b -> blobPath b == p) (traceShowId projectBlobs)
  in if
    | p == "/dev/null"  -> pure Nothing
    | isJust candidate  -> pure candidate
    | otherwise         -> throwError (SomeException (FileNotFound p))
