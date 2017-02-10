module SemanticDiffPar where

import Arguments
import qualified Control.Monad.Par.IO as ParIO
import Control.Monad.Reader
import qualified Data.Text as T
import Prologue
import qualified Renderer as R
import SemanticDiff

fetchDiffs :: Arguments -> IO [T.Text]
fetchDiffs args@Arguments{..} = pure . pure . R.concatOutputs =<< (ParIO.runParIO . liftIO $ for filePaths (fetchDiff args))
