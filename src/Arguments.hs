module Arguments (Arguments(..), ExtraArg(..), programArguments, args) where

import Data.Functor.Both
import Data.Maybe
import Data.Text
import Prologue hiding ((<>))
import Prelude
import System.Environment
import System.Directory
import System.IO.Error (IOError)

import qualified Renderer as R

data ExtraArg = ShaPair (Both (Maybe String)) | FileArg FilePath deriving (Show)

-- | The command line arguments to the application.
data Arguments =
   -- | Arguments for optparse-applicative
  CmdLineArguments
  { format :: R.Format
  , maybeTimeout :: Maybe Float
  , output :: Maybe FilePath
  , noIndex :: Bool
  , extraArgs :: [ExtraArg]
  }
  -- | Arguments actually used by the program (includes command line, environment, and default args).
  | Arguments
  { gitDir :: FilePath
  , alternateObjectDirs :: [Text]
  , format :: R.Format
  , timeoutInMicroseconds :: Int
  , output :: Maybe FilePath
  , noIndex :: Bool
  , shaRange :: Both (Maybe String)
  , filePaths :: [FilePath]
  } deriving (Show)

-- | Returns Arguments for the program from parsed command line arguments.
programArguments :: Arguments -> IO Arguments
programArguments args@Arguments{..} = pure args
programArguments CmdLineArguments{..} = do
  pwd <- getCurrentDirectory
  gitDir <- fromMaybe pwd <$> lookupEnv "GIT_DIR"
  eitherObjectDirs <- try $ parseObjectDirs . toS <$> getEnv "GIT_ALTERNATE_OBJECT_DIRECTORIES"
  let alternateObjectDirs = case (eitherObjectDirs :: Either IOError [Text]) of
                              (Left _) -> []
                              (Right objectDirs) -> objectDirs
  pure Arguments
    { gitDir = gitDir
    , alternateObjectDirs = alternateObjectDirs
    , format = format
    , timeoutInMicroseconds = maybe defaultTimeout toMicroseconds maybeTimeout
    , output = output
    , noIndex = noIndex
    , shaRange = fetchShas extraArgs
    , filePaths = fetchPaths extraArgs
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

-- | Quickly assemble an Arguments data record with defaults.
args :: FilePath -> String -> String -> [String] -> R.Format -> Arguments
args gitDir sha1 sha2 filePaths format = Arguments
  { gitDir =  gitDir
  , alternateObjectDirs = []
  , format = format
  , timeoutInMicroseconds = defaultTimeout
  , output = Nothing
  , noIndex = False
  , shaRange = Just <$> both sha1 sha2
  , filePaths = filePaths
  }

-- | 7 seconds
defaultTimeout :: Int
defaultTimeout = 7 * 1000000

toMicroseconds :: Float -> Int
toMicroseconds num = floor $ num * 1000000

parseObjectDirs :: Text -> [Text]
parseObjectDirs = split (== ':')
