module Arguments (Arguments(..), ExtraArg(..), args, filePathsFromArgs, maybeShasFromArgs) where

import Data.Functor.Both
import Data.Monoid
import Prelude
import qualified Renderer as R

data ExtraArg = ShaPair (Both (Maybe String)) | FileArg FilePath deriving (Show)

-- | The command line arguments to the application.
data Arguments = Arguments
  { format :: R.Format
  , maybeTimeout :: Maybe Float
  , output :: Maybe FilePath
  , noIndex :: Bool
  , extraArgs :: [ExtraArg]
  } deriving (Show)

args :: String -> String -> [String] -> R.Format -> Arguments
args sha1 sha2 filePaths format = Arguments
  { format = format
  , maybeTimeout = Just 10.0
  , output = Nothing
  , noIndex = False
  , extraArgs = [ShaPair (Just <$> both sha1 sha2)] <> (FileArg <$> filePaths)
  }

filePathsFromArgs :: Arguments -> [FilePath]
filePathsFromArgs Arguments{..} = go extraArgs
  where
    go [] = []
    go (FileArg x:xs) = x : go xs
    go (_:xs) = go xs

maybeShasFromArgs :: Arguments -> Both (Maybe String)
maybeShasFromArgs Arguments{..} = go extraArgs
  where
    go [] = both Nothing Nothing
    go (ShaPair x:_) = x
    go (_:xs) = go xs
