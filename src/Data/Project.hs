{-# LANGUAGE DeriveAnyClass #-}
module Data.Project where

import Data.Blob
import Data.Language
import qualified Data.Text as T
import Prologue
import Proto3.Suite
import System.FilePath.Posix

data Project blobs paths path = Project
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

type Concrete = Project [] [] FilePath
type PB = Project NestedVec UnpackedVec Text

fromPB :: PB -> Concrete
fromPB Project {..} = Project
  { projectRootDir     = T.unpack projectRootDir
  , projectBlobs       = go projectBlobs
  , projectLanguage    = projectLanguage
  , projectEntryPaths  = T.unpack <$> go projectEntryPaths
  , projectExcludeDirs = T.unpack <$> go projectExcludeDirs
  } where go :: Foldable f => f a -> [a]
          go = foldr (:) []

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

toFile :: Blob -> File
toFile (Blob _ p l) = File p l
