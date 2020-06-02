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
import qualified System.Path as Path

-- | A 'Project' contains all the information that semantic needs
-- to execute an analysis, diffing, or graphing pass.
data Project = Project
  { projectRootDir     :: Path.AbsRelDir
  , projectBlobs       :: [Blob]
  , projectLanguage    :: Language
  , projectExcludeDirs :: [Path.AbsRelDir]
  } deriving (Eq, Show)

projectName :: Project -> Text
projectName = T.pack . maybe "" Path.toString . Path.takeDirName . projectRootDir

projectExtensions :: Project -> [String]
projectExtensions = extensionsForLanguage . projectLanguage

projectFiles :: Project -> [File Language]
projectFiles = fmap blobFile . projectBlobs
