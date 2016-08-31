module Arguments (Arguments(..), args) where

import Data.Functor.Both
import qualified Prelude as P
import Prelude
import Prologue
import qualified Renderer as R

-- | The command line arguments to the application.
data Arguments = Arguments {
  format :: R.Format,
  maybeShas :: Both (Maybe P.String),
  maybeTimeout :: Maybe Float,
  output :: Maybe FilePath,
  filepaths :: [FilePath] }
  deriving (Show)

args :: String -> String -> [String] -> R.Format -> Arguments
args sha1 sha2 filePaths format = Arguments { format = format
                                            , maybeShas = Just <$> both sha1 sha2
                                            , filepaths = filePaths
                                            , maybeTimeout = Just 10.0
                                            , output = Nothing
                                            }
