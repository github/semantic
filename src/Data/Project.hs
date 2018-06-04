{-# LANGUAGE MultiWayIf #-}

module Data.Project where

import           Data.ByteString.Char8 as BC (pack, writeFile)
import           Data.Blob
import           Data.Language
import           Prologue
import           System.FilePath.Posix

data Project = Project
  { projectRootDir :: FilePath
  , projectFiles :: [File]
  , projectLanguage :: Language
  , projectEntryPoints :: [File]
  , projectExcludeDirs :: [FilePath]
  }
  deriving (Eq, Ord, Show)

projectName :: Project -> ByteString
projectName = BC.pack . dropExtensions . takeFileName . projectRootDir

projectExtensions :: Project -> [String]
projectExtensions = extensionsForLanguage . projectLanguage


data File = File
  { filePath     :: FilePath
  , fileLanguage :: Language
  }
  deriving (Eq, Ord, Show)

file :: FilePath -> File
file path = File path (languageForFilePath path)
  where languageForFilePath = languageForType . takeExtension

blobToFile :: Blob -> File
blobToFile (Blob _ f l) = File f l
