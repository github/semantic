module Arguments (Arguments(..)) where

import Data.Functor.Both
import qualified Prelude as P
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
