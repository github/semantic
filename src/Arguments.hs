{-# OPTIONS_GHC -funbox-strict-fields #-}
module Arguments (Arguments(..), CmdLineOptions(..), DiffMode(..), ExtraArg(..), RunMode(..), programArguments, args, diffPathsArgs, parseArgs) where

import Data.Functor.Both
import Data.Maybe
import Data.Text
import Prologue hiding ((<>))
import Prelude
import System.Environment
import System.Directory
import System.FilePath.Posix (takeFileName, (-<.>))
import System.IO.Error (IOError)

import qualified Renderer as R

data ExtraArg = ShaPair (Both (Maybe String))
              | FileArg FilePath
              deriving (Show)

data DiffMode = PathDiff (Both FilePath)
              | CommitDiff
              deriving (Show)

data RunMode = Diff
             | Parse
             deriving (Show)

-- | The command line options to the application (arguments for optparse-applicative).
data CmdLineOptions = CmdLineOptions
  { outputFormat :: R.Format
  , maybeTimeout :: Maybe Float
  , outputFilePath :: Maybe FilePath
  , noIndex :: Bool
  , extraArgs :: [ExtraArg]
  , developmentMode' :: Bool
  , runMode' :: RunMode
  }

-- | Arguments for the program (includes command line, environment, and defaults).
data Arguments = Arguments
  { gitDir :: FilePath
  , alternateObjectDirs :: [Text]
  , format :: R.Format
  , timeoutInMicroseconds :: Int
  , output :: Maybe FilePath
  , diffMode :: DiffMode
  , runMode :: RunMode
  , shaRange :: Both (Maybe String)
  , filePaths :: [FilePath]
  , developmentMode :: Bool
  } deriving (Show)

-- | Returns Arguments for the program from parsed command line arguments.
programArguments :: CmdLineOptions -> IO Arguments
programArguments CmdLineOptions{..} = do
  pwd <- getCurrentDirectory
  gitDir <- fromMaybe pwd <$> lookupEnv "GIT_DIR"
  eitherObjectDirs <- try $ parseObjectDirs . toS <$> getEnv "GIT_ALTERNATE_OBJECT_DIRECTORIES"
  output <- getOutputPath outputFilePath
  let alternateObjectDirs = case (eitherObjectDirs :: Either IOError [Text]) of
                              (Left _) -> []
                              (Right objectDirs) -> objectDirs

  let filePaths = fetchPaths extraArgs
  pure Arguments
    { gitDir = gitDir
    , alternateObjectDirs = alternateObjectDirs
    , format = outputFormat
    , timeoutInMicroseconds = maybe defaultTimeout toMicroseconds maybeTimeout
    , output = output
    , diffMode = case (noIndex, filePaths) of
      (True, [fileA, fileB]) -> PathDiff (both fileA fileB)
      (_, _) -> CommitDiff
    , runMode = runMode'
    , shaRange = fetchShas extraArgs
    , filePaths = filePaths
    , developmentMode = developmentMode'
    }
  where
    fetchPaths :: [ExtraArg] -> [FilePath]
    fetchPaths [] = []
    fetchPaths (FileArg x:xs) = x : fetchPaths xs
    fetchPaths (_:xs) = fetchPaths xs

    fetchShas :: [ExtraArg] -> Both (Maybe String)
    fetchShas [] = both Nothing Nothing
    fetchShas (ShaPair x:_) = x
    fetchShas (_:xs) = fetchShas xs

    getOutputPath Nothing = pure Nothing
    getOutputPath (Just path) = do
      isDir <- doesDirectoryExist path
      pure . Just $ if isDir then takeFileName path -<.> ".html" else path


-- | Quickly assemble an Arguments data record with defaults.
args :: FilePath -> String -> String -> [String] -> R.Format -> Arguments
args gitDir sha1 sha2 filePaths format = Arguments
  { gitDir = gitDir
  , alternateObjectDirs = []
  , format = format
  , timeoutInMicroseconds = defaultTimeout
  , output = Nothing
  , diffMode = CommitDiff
  , runMode = Diff
  , shaRange = Just <$> both sha1 sha2
  , filePaths = filePaths
  , developmentMode = False
  }

diffPathsArgs :: FilePath -> Both FilePath -> R.Format -> Arguments
diffPathsArgs gitDir paths format = Arguments
  { gitDir = gitDir
  , alternateObjectDirs = []
  , format = format
  , timeoutInMicroseconds = defaultTimeout
  , output = Nothing
  , diffMode = PathDiff paths
  , runMode = Diff
  , shaRange = both Nothing Nothing
  , filePaths = []
  , developmentMode = False
  }

parseArgs :: [String] -> R.Format -> Arguments
parseArgs filePaths format = Arguments
  { gitDir = ""
  , alternateObjectDirs = []
  , format = format
  , timeoutInMicroseconds = defaultTimeout
  , output = Nothing
  , diffMode = CommitDiff
  , runMode = Parse
  , shaRange = both Nothing Nothing
  , filePaths = filePaths
  , developmentMode = False
  }

-- | 7 seconds
defaultTimeout :: Int
defaultTimeout = 7 * 1000000

toMicroseconds :: Float -> Int
toMicroseconds num = floor $ num * 1000000

parseObjectDirs :: Text -> [Text]
parseObjectDirs = split (== ':')
