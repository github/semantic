module Analysis.Project
  ( Project (..)
  , projectExtensions
  , projectName
  , projectFiles
  ) where

import Prelude hiding (readFile)

import           Analysis.Blob
import           Analysis.File
import           Data.Text (Text)
import qualified Data.Text as T
import           Source.Language
import           System.FilePath

-- | A 'Project' contains all the information that semantic needs
-- to execute an analysis, diffing, or graphing pass.
data Project = Project
  { projectRootDir     :: FilePath
  , projectBlobs       :: [Blob]
  , projectLanguage    :: Language
  , projectExcludeDirs :: [FilePath]
  } deriving (Eq, Show)

projectName :: Project -> Text
projectName = T.pack . takeDirectory . projectRootDir

projectExtensions :: Project -> [String]
projectExtensions = extensionsForLanguage . projectLanguage

projectFiles :: Project -> [File Language]
projectFiles = fmap blobFile . projectBlobs
