module Arguments (Arguments(..), args) where

import Data.Functor.Both
import qualified Prelude as P
import Prelude
import qualified Renderer as R

-- | The command line arguments to the application.
data Arguments = Arguments {
  format :: R.Format,
  maybeShas :: Both (Maybe P.String),
  maybeTimeout :: Maybe Float,
  output :: Maybe FilePath,
  filePaths :: [FilePath] }
  deriving (Show)

args :: String -> String -> [FilePath] -> R.Format -> Arguments
args sha1 sha2 paths format = Arguments { format = format
                                            , maybeShas = Just <$> both sha1 sha2
                                            , filePaths = paths
                                            , maybeTimeout = Just 10.0
                                            , output = Nothing
                                            }
