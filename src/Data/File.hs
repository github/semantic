module Data.File where

import           Data.ByteString.Char8 as BC (pack)
import           Data.Language
import qualified Data.List.NonEmpty as NonEmpty
import           Prologue
import           System.FilePath.Posix

data File = File
  { filePath :: FilePath
  , fileLanguage :: Maybe Language
  }
  deriving (Eq, Ord, Show)

file :: FilePath -> File
file path = File path (languageForFilePath path)
  where languageForFilePath = languageForType . takeExtension

data Project = Project
  { projectEntryPoints :: NonEmpty File
  , projectRootDir :: FilePath
  , projectFiles :: [File]
  }
  deriving (Eq, Ord, Show)

projectAllFiles :: Project -> [File]
projectAllFiles Project{..} = NonEmpty.toList projectEntryPoints <> projectFiles

projectName :: Project -> ByteString
projectName = BC.pack . dropExtensions . takeFileName . projectRootDir

projectLanguage :: Project -> Maybe Language
projectLanguage = fileLanguage. projectEntryPoint

projectEntryPoint :: Project -> File
projectEntryPoint = NonEmpty.head . projectEntryPoints

projectExtensions :: Project -> [String]
projectExtensions = extensionsForLanguage . projectLanguage
