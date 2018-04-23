module Data.File where

import           Data.ByteString.Char8 as BC (pack)
import           Data.Language
import           Prologue
import           System.FilePath.Posix

data File = File
  { filePath :: FilePath
  , fileLanguage :: Maybe Language
  }
  deriving (Eq, Ord, Show)

data Project = Project
  { projectRootDir :: FilePath
  , projectFiles :: [File]
  , projectLanguage :: Language
  }
  deriving (Eq, Ord, Show)

file :: FilePath -> File
file path = File path (languageForFilePath path)
  where languageForFilePath = languageForType . takeExtension

projectName :: Project -> ByteString
projectName = BC.pack . dropExtensions . takeFileName . projectRootDir

projectExtensions :: Project -> [String]
projectExtensions = extensionsForLanguage . projectLanguage
