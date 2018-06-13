module Data.Project where

import Data.Text as T (pack)
import Data.Language
import Prologue
import System.FilePath.Posix

data Project = Project
  { projectRootDir     :: FilePath
  , projectFiles       :: [File]
  , projectLanguage    :: Language
  , projectEntryPoints :: [File]
  , projectExcludeDirs :: [FilePath]
  }
  deriving (Eq, Ord, Show)

projectName :: Project -> Text
projectName = T.pack . dropExtensions . takeFileName . projectRootDir

projectExtensions :: Project -> [String]
projectExtensions = extensionsForLanguage . projectLanguage


data File = File
  { filePath     :: FilePath
  , fileLanguage :: Language
  } deriving (Eq, Ord, Show)

file :: FilePath -> File
file path = File path (languageForFilePath path)
  where languageForFilePath = languageForType . takeExtension
