module Data.File where

import           Data.ByteString.Char8 as BC (pack)
import           Data.Language
import qualified Data.List.NonEmpty as NonEmpty
import           Prologue
import           System.FilePath.Posix

data File = File
  { filePath :: FilePath
  , fileLanguage :: Language
  }
  deriving (Eq, Ord, Show)

data Project = Project
  { projectRootDir :: FilePath
  , projectFiles :: [File]
  , projectLanguage :: Language
  }
  deriving (Eq, Ord, Show)

projectName :: Project -> ByteString
projectName = BC.pack . dropExtensions . takeFileName . projectRootDir

projectExtensions :: Project -> [String]
projectExtensions = extensionsForLanguage . projectLanguage
